#!/usr/bin/env python3
"""Generate a medieval castle as a glTF binary (.glb) file with procedural stone textures."""

import struct
import json
import math
import random
import io
import os
from PIL import Image

# --- Constants ---
TEXTURE_SCALE = 1.0 / 4.0  # one texture repeat every 4 meters

# glTF component types
FLOAT = 5126
UNSIGNED_SHORT = 5123
UNSIGNED_INT = 5125

# glTF buffer view targets
ARRAY_BUFFER = 34962
ELEMENT_ARRAY_BUFFER = 34963

# Texture sampler constants
REPEAT = 10497
LINEAR = 9729
LINEAR_MIPMAP_LINEAR = 9987


class GltfBuilder:
    """Accumulates geometry, materials, textures and outputs a .glb file."""

    def __init__(self):
        self.buffer = bytearray()
        self.buffer_views = []
        self.accessors = []
        self.meshes = []
        self.nodes = []
        self.materials = []
        self.textures = []
        self.images = []
        self.samplers = []

    def _align4(self):
        padding = (4 - len(self.buffer) % 4) % 4
        self.buffer.extend(b'\x00' * padding)

    def add_buffer_view(self, data, target=None):
        self._align4()
        offset = len(self.buffer)
        self.buffer.extend(data)
        bv = {"buffer": 0, "byteOffset": offset, "byteLength": len(data)}
        if target is not None:
            bv["target"] = target
        idx = len(self.buffer_views)
        self.buffer_views.append(bv)
        return idx

    def add_accessor(self, buffer_view, component_type, count, type_str,
                     min_vals=None, max_vals=None):
        acc = {
            "bufferView": buffer_view,
            "componentType": component_type,
            "count": count,
            "type": type_str,
        }
        if min_vals is not None:
            acc["min"] = min_vals
        if max_vals is not None:
            acc["max"] = max_vals
        idx = len(self.accessors)
        self.accessors.append(acc)
        return idx

    def add_image_from_pil(self, pil_image):
        buf = io.BytesIO()
        pil_image.save(buf, format='PNG')
        png_bytes = buf.getvalue()
        bv_idx = self.add_buffer_view(png_bytes)
        img = {"bufferView": bv_idx, "mimeType": "image/png"}
        idx = len(self.images)
        self.images.append(img)
        return idx

    def add_sampler(self, wrap_s=REPEAT, wrap_t=REPEAT,
                    mag_filter=LINEAR, min_filter=LINEAR_MIPMAP_LINEAR):
        s = {
            "magFilter": mag_filter,
            "minFilter": min_filter,
            "wrapS": wrap_s,
            "wrapT": wrap_t,
        }
        idx = len(self.samplers)
        self.samplers.append(s)
        return idx

    def add_texture(self, image_index, sampler_index):
        t = {"source": image_index, "sampler": sampler_index}
        idx = len(self.textures)
        self.textures.append(t)
        return idx

    def add_material(self, name, texture_index, metallic=0.0, roughness=0.85):
        mat = {
            "name": name,
            "pbrMetallicRoughness": {
                "baseColorTexture": {"index": texture_index},
                "metallicFactor": metallic,
                "roughnessFactor": roughness,
            }
        }
        idx = len(self.materials)
        self.materials.append(mat)
        return idx

    def add_mesh(self, positions, normals, uvs, indices, material_index):
        # Pack positions
        pos_data = struct.pack('<' + 'f' * len(positions), *positions)
        pos_bv = self.add_buffer_view(pos_data, ARRAY_BUFFER)
        n_verts = len(positions) // 3
        # Compute min/max for positions
        xs = positions[0::3]
        ys = positions[1::3]
        zs = positions[2::3]
        pos_acc = self.add_accessor(
            pos_bv, FLOAT, n_verts, "VEC3",
            [min(xs), min(ys), min(zs)],
            [max(xs), max(ys), max(zs)]
        )

        # Pack normals
        norm_data = struct.pack('<' + 'f' * len(normals), *normals)
        norm_bv = self.add_buffer_view(norm_data, ARRAY_BUFFER)
        norm_acc = self.add_accessor(norm_bv, FLOAT, n_verts, "VEC3")

        # Pack UVs
        uv_data = struct.pack('<' + 'f' * len(uvs), *uvs)
        uv_bv = self.add_buffer_view(uv_data, ARRAY_BUFFER)
        uv_acc = self.add_accessor(uv_bv, FLOAT, n_verts, "VEC2")

        # Pack indices
        if max(indices) > 65535:
            idx_data = struct.pack('<' + 'I' * len(indices), *indices)
            comp_type = UNSIGNED_INT
        else:
            idx_data = struct.pack('<' + 'H' * len(indices), *indices)
            comp_type = UNSIGNED_SHORT
        idx_bv = self.add_buffer_view(idx_data, ELEMENT_ARRAY_BUFFER)
        idx_acc = self.add_accessor(idx_bv, comp_type, len(indices), "SCALAR")

        mesh = {
            "primitives": [{
                "attributes": {
                    "POSITION": pos_acc,
                    "NORMAL": norm_acc,
                    "TEXCOORD_0": uv_acc,
                },
                "indices": idx_acc,
                "material": material_index,
            }]
        }
        idx = len(self.meshes)
        self.meshes.append(mesh)
        return idx

    def add_node(self, mesh_index=None, name=None, translation=None):
        node = {}
        if mesh_index is not None:
            node["mesh"] = mesh_index
        if name:
            node["name"] = name
        if translation:
            node["translation"] = translation
        idx = len(self.nodes)
        self.nodes.append(node)
        return idx

    def build_glb(self):
        self._align4()
        gltf = {
            "asset": {"version": "2.0", "generator": "castle_builder.py"},
            "scene": 0,
            "scenes": [{"nodes": list(range(len(self.nodes)))}],
            "nodes": self.nodes,
            "meshes": self.meshes,
            "accessors": self.accessors,
            "bufferViews": self.buffer_views,
            "buffers": [{"byteLength": len(self.buffer)}],
            "materials": self.materials,
            "textures": self.textures,
            "images": self.images,
            "samplers": self.samplers,
        }
        json_str = json.dumps(gltf, separators=(',', ':'))
        json_bytes = json_str.encode('utf-8')
        # Pad JSON to 4-byte alignment
        json_pad = (4 - len(json_bytes) % 4) % 4
        json_bytes += b' ' * json_pad

        bin_data = bytes(self.buffer)
        # Pad BIN to 4-byte alignment
        bin_pad = (4 - len(bin_data) % 4) % 4
        bin_data += b'\x00' * bin_pad

        total_length = 12 + 8 + len(json_bytes) + 8 + len(bin_data)

        out = bytearray()
        # Header
        out.extend(struct.pack('<III', 0x46546C67, 2, total_length))
        # JSON chunk
        out.extend(struct.pack('<II', len(json_bytes), 0x4E4F534A))
        out.extend(json_bytes)
        # BIN chunk
        out.extend(struct.pack('<II', len(bin_data), 0x004E4942))
        out.extend(bin_data)
        return bytes(out)


# --- Geometry Generators ---

def generate_box(width, height, depth, pos=(0, 0, 0)):
    """Generate a box with 24 vertices (4 per face), proper normals and world-space UVs."""
    hw, hh, hd = width / 2, height / 2, depth / 2
    px, py, pz = pos

    positions = []
    normals = []
    uvs = []
    indices = []

    # Face definitions: (normal, 4 corners, uv axes)
    faces = [
        # Front face (+Z)
        ((0, 0, 1), [
            (-hw, -hh, hd), (hw, -hh, hd), (hw, hh, hd), (-hw, hh, hd)
        ], (0, 1)),  # u=x, v=y
        # Back face (-Z)
        ((0, 0, -1), [
            (hw, -hh, -hd), (-hw, -hh, -hd), (-hw, hh, -hd), (hw, hh, -hd)
        ], (0, 1)),
        # Right face (+X)
        ((1, 0, 0), [
            (hw, -hh, hd), (hw, -hh, -hd), (hw, hh, -hd), (hw, hh, hd)
        ], (2, 1)),  # u=z, v=y
        # Left face (-X)
        ((-1, 0, 0), [
            (-hw, -hh, -hd), (-hw, -hh, hd), (-hw, hh, hd), (-hw, hh, -hd)
        ], (2, 1)),
        # Top face (+Y)
        ((0, 1, 0), [
            (-hw, hh, hd), (hw, hh, hd), (hw, hh, -hd), (-hw, hh, -hd)
        ], (0, 2)),  # u=x, v=z
        # Bottom face (-Y)
        ((0, -1, 0), [
            (-hw, -hh, -hd), (hw, -hh, -hd), (hw, -hh, hd), (-hw, -hh, hd)
        ], (0, 2)),
    ]

    base_idx = 0
    for normal, corners, (u_axis, v_axis) in faces:
        for corner in corners:
            # World position
            wx = corner[0] + px
            wy = corner[1] + py
            wz = corner[2] + pz
            positions.extend([wx, wy, wz])
            normals.extend(normal)
            # World-space UVs
            world_coords = [wx, wy, wz]
            u = world_coords[u_axis] * TEXTURE_SCALE
            v = world_coords[v_axis] * TEXTURE_SCALE
            uvs.extend([u, v])
        # Two triangles per face (CCW winding)
        indices.extend([
            base_idx, base_idx + 1, base_idx + 2,
            base_idx, base_idx + 2, base_idx + 3,
        ])
        base_idx += 4

    return positions, normals, uvs, indices


def generate_cylinder(radius, height, segments, pos=(0, 0, 0)):
    """Generate a cylinder with proper normals and world-space UVs."""
    px, py, pz = pos
    positions = []
    normals = []
    uvs = []
    indices = []
    base_idx = 0

    circumference = 2 * math.pi * radius

    # Side faces
    for i in range(segments):
        a0 = 2 * math.pi * i / segments
        a1 = 2 * math.pi * (i + 1) / segments
        c0, s0 = math.cos(a0), math.sin(a0)
        c1, s1 = math.cos(a1), math.sin(a1)

        # 4 vertices per quad (ordered for CCW winding when viewed from outside)
        # 0: bottom at a1, 1: bottom at a0, 2: top at a0, 3: top at a1
        verts = [
            (radius * c1, 0, radius * s1),
            (radius * c0, 0, radius * s0),
            (radius * c0, height, radius * s0),
            (radius * c1, height, radius * s1),
        ]
        norms = [
            (c1, 0, s1),
            (c0, 0, s0),
            (c0, 0, s0),
            (c1, 0, s1),
        ]

        for vi, v in enumerate(verts):
            positions.extend([v[0] + px, v[1] + py, v[2] + pz])
            normals.extend(norms[vi])
            # UV: u based on arc position, v based on height
            if vi in (0, 3):
                u = (circumference * (i + 1) / segments) * TEXTURE_SCALE
            else:
                u = (circumference * i / segments) * TEXTURE_SCALE
            v_coord = (v[1] + py) * TEXTURE_SCALE
            uvs.extend([u, v_coord])

        indices.extend([
            base_idx, base_idx + 1, base_idx + 2,
            base_idx, base_idx + 2, base_idx + 3,
        ])
        base_idx += 4

    # Top cap
    center_idx = base_idx
    positions.extend([px, py + height, pz])
    normals.extend([0, 1, 0])
    uvs.extend([0.5, 0.5])
    base_idx += 1
    for i in range(segments):
        a = 2 * math.pi * i / segments
        cx, cz = radius * math.cos(a), radius * math.sin(a)
        positions.extend([cx + px, py + height, cz + pz])
        normals.extend([0, 1, 0])
        uvs.extend([0.5 + 0.5 * math.cos(a), 0.5 + 0.5 * math.sin(a)])
        base_idx += 1

    for i in range(segments):
        i_next = (i + 1) % segments
        indices.extend([center_idx, center_idx + 1 + i, center_idx + 1 + i_next])

    # Bottom cap
    center_idx = base_idx
    positions.extend([px, py, pz])
    normals.extend([0, -1, 0])
    uvs.extend([0.5, 0.5])
    base_idx += 1
    for i in range(segments):
        a = 2 * math.pi * i / segments
        cx, cz = radius * math.cos(a), radius * math.sin(a)
        positions.extend([cx + px, py, cz + pz])
        normals.extend([0, -1, 0])
        uvs.extend([0.5 + 0.5 * math.cos(a), 0.5 + 0.5 * math.sin(a)])
        base_idx += 1

    for i in range(segments):
        i_next = (i + 1) % segments
        indices.extend([center_idx, center_idx + 1 + i_next, center_idx + 1 + i])

    return positions, normals, uvs, indices


def merge_geometry(*geom_list):
    """Merge multiple (positions, normals, uvs, indices) tuples into one."""
    all_pos = []
    all_norm = []
    all_uv = []
    all_idx = []
    vertex_offset = 0
    for pos, norm, uv, idx in geom_list:
        all_pos.extend(pos)
        all_norm.extend(norm)
        all_uv.extend(uv)
        all_idx.extend(i + vertex_offset for i in idx)
        vertex_offset += len(pos) // 3
    return all_pos, all_norm, all_uv, all_idx


# --- Texture Generator ---

def generate_stone_texture(width=512, height=512):
    """Generate a procedural stone wall texture."""
    random.seed(42)
    img = Image.new('RGB', (width, height))
    pixels = img.load()

    # Mortar color
    mortar_r, mortar_g, mortar_b = 120, 110, 100

    # Fill with mortar
    for y in range(height):
        for x in range(width):
            pixels[x, y] = (
                mortar_r + random.randint(-5, 5),
                mortar_g + random.randint(-5, 5),
                mortar_b + random.randint(-5, 5),
            )

    # Stone block dimensions
    block_w = 64
    block_h = 32
    mortar_gap = 3

    # Stone color palettes
    palettes = [
        (190, 180, 160, 20),  # light sandstone
        (165, 160, 155, 15),  # medium gray
        (140, 135, 130, 15),  # dark gray
        (155, 175, 145, 12),  # mossy
    ]
    weights = [0.40, 0.35, 0.15, 0.10]

    rows = height // block_h
    cols = width // block_w

    for row in range(rows):
        offset = (block_w // 2) if (row % 2 == 1) else 0
        for col in range(cols + 1):
            # Pick stone color
            r = random.random()
            cumulative = 0
            palette = palettes[0]
            for p, w in zip(palettes, weights):
                cumulative += w
                if r <= cumulative:
                    palette = p
                    break
            base_r, base_g, base_b, variation = palette

            x0 = col * block_w + offset
            y0 = row * block_h
            x1 = x0 + block_w - mortar_gap
            y1 = y0 + block_h - mortar_gap

            for sy in range(max(0, y0), min(height, y1)):
                for sx in range(x0 % width, min(width, x1)):
                    if sx < 0:
                        continue
                    # Wrap x for offset rows
                    actual_x = sx % width
                    nr = max(0, min(255, base_r + random.randint(-variation, variation)))
                    ng = max(0, min(255, base_g + random.randint(-variation, variation)))
                    nb = max(0, min(255, base_b + random.randint(-variation, variation)))
                    pixels[actual_x, sy] = (nr, ng, nb)

                    # Handle wrapping for offset rows that extend past the right edge
                    if x1 > width and sx >= x0 % width:
                        wrapped_x = sx - width + offset
                        if 0 <= wrapped_x < width:
                            pixels[wrapped_x, sy] = (nr, ng, nb)

    return img


# --- Castle Builder ---

def build_castle_geometry():
    """Build the complete castle geometry."""
    parts = []

    # --- Corner Towers ---
    tower_radius = 3.0
    tower_height = 12.0
    tower_segments = 16
    tower_positions = [
        (18, 0, 18), (-18, 0, 18), (18, 0, -18), (-18, 0, -18)
    ]
    for tp in tower_positions:
        parts.append(generate_cylinder(tower_radius, tower_height, tower_segments, tp))
        # Crenellations on towers
        for ci in range(8):
            angle = 2 * math.pi * ci / 8
            mx = tp[0] + (tower_radius - 0.4) * math.cos(angle)
            mz = tp[2] + (tower_radius - 0.4) * math.sin(angle)
            parts.append(generate_box(0.8, 1.5, 0.8, (mx, tower_height + 0.75, mz)))

    # --- Walls ---
    wall_height = 8.0
    wall_thickness = 1.5

    # North wall (z = -18, from x=-18 to x=18)
    parts.append(generate_box(36, wall_height, wall_thickness, (0, wall_height / 2, -18)))
    # East wall (x = 18, from z=-18 to z=18)
    parts.append(generate_box(wall_thickness, wall_height, 36, (18, wall_height / 2, 0)))
    # West wall (x = -18, from z=-18 to z=18)
    parts.append(generate_box(wall_thickness, wall_height, 36, (-18, wall_height / 2, 0)))

    # South wall split for gate (z = 18)
    gate_half_width = 3.0
    south_left_len = 18 - gate_half_width - tower_radius
    south_right_len = south_left_len
    left_center_x = -18 + tower_radius + south_left_len / 2
    right_center_x = 18 - tower_radius - south_right_len / 2
    parts.append(generate_box(south_left_len, wall_height, wall_thickness,
                              (left_center_x, wall_height / 2, 18)))
    parts.append(generate_box(south_right_len, wall_height, wall_thickness,
                              (right_center_x, wall_height / 2, 18)))

    # Wall battlements (merlons on top)
    merlon_w = 1.0
    merlon_h = 1.5
    merlon_spacing = 3.0

    # North wall merlons
    for i in range(12):
        mx = -16.5 + i * merlon_spacing
        parts.append(generate_box(merlon_w, merlon_h, wall_thickness,
                                  (mx, wall_height + merlon_h / 2, -18)))
    # East wall merlons
    for i in range(12):
        mz = -16.5 + i * merlon_spacing
        parts.append(generate_box(wall_thickness, merlon_h, merlon_w,
                                  (18, wall_height + merlon_h / 2, mz)))
    # West wall merlons
    for i in range(12):
        mz = -16.5 + i * merlon_spacing
        parts.append(generate_box(wall_thickness, merlon_h, merlon_w,
                                  (-18, wall_height + merlon_h / 2, mz)))
    # South wall merlons (skip gate area)
    for i in range(12):
        mx = -16.5 + i * merlon_spacing
        if abs(mx) < gate_half_width + 1:
            continue
        parts.append(generate_box(merlon_w, merlon_h, wall_thickness,
                                  (mx, wall_height + merlon_h / 2, 18)))

    # --- Gatehouse ---
    # Two flanking pillars
    pillar_size = 2.0
    pillar_height = 10.0
    parts.append(generate_box(pillar_size, pillar_height, pillar_size,
                              (-gate_half_width - 0.5, pillar_height / 2, 18)))
    parts.append(generate_box(pillar_size, pillar_height, pillar_size,
                              (gate_half_width + 0.5, pillar_height / 2, 18)))
    # Lintel over gate
    parts.append(generate_box(gate_half_width * 2 + pillar_size, 2.0, pillar_size,
                              (0, pillar_height - 1.0, 18)))
    # Gatehouse merlons
    for i in range(3):
        mx = -2.5 + i * 2.5
        parts.append(generate_box(1.0, 1.5, 1.5,
                                  (mx, pillar_height + 0.75, 18)))

    # --- Central Keep ---
    keep_w = 8.0
    keep_h = 16.0
    keep_d = 8.0
    parts.append(generate_box(keep_w, keep_h, keep_d, (0, keep_h / 2, 0)))

    # Keep battlements
    for side in range(4):
        for i in range(3):
            offset = -2.5 + i * 2.5
            if side == 0:  # front
                parts.append(generate_box(1.0, 1.5, 0.8,
                                          (offset, keep_h + 0.75, keep_d / 2)))
            elif side == 1:  # back
                parts.append(generate_box(1.0, 1.5, 0.8,
                                          (offset, keep_h + 0.75, -keep_d / 2)))
            elif side == 2:  # right
                parts.append(generate_box(0.8, 1.5, 1.0,
                                          (keep_w / 2, keep_h + 0.75, offset)))
            else:  # left
                parts.append(generate_box(0.8, 1.5, 1.0,
                                          (-keep_w / 2, keep_h + 0.75, offset)))

    # Keep top turret
    parts.append(generate_cylinder(1.5, 4.0, 12, (0, keep_h, 0)))
    # Turret crenellations
    for ci in range(6):
        angle = 2 * math.pi * ci / 6
        mx = 1.1 * math.cos(angle)
        mz = 1.1 * math.sin(angle)
        parts.append(generate_box(0.6, 1.0, 0.6,
                                  (mx, keep_h + 4.0 + 0.5, mz)))

    return merge_geometry(*parts)


def main():
    print("Generating stone texture...")
    stone_tex = generate_stone_texture()

    print("Building castle geometry...")
    positions, normals, uvs, indices = build_castle_geometry()
    n_verts = len(positions) // 3
    n_tris = len(indices) // 3
    print(f"  {n_verts} vertices, {n_tris} triangles")

    print("Assembling glTF...")
    builder = GltfBuilder()

    sampler_idx = builder.add_sampler()
    image_idx = builder.add_image_from_pil(stone_tex)
    texture_idx = builder.add_texture(image_idx, sampler_idx)
    material_idx = builder.add_material("StoneMaterial", texture_idx,
                                         metallic=0.0, roughness=0.85)

    mesh_idx = builder.add_mesh(positions, normals, uvs, indices, material_idx)
    builder.add_node(mesh_idx, name="Castle")

    glb_data = builder.build_glb()

    output_path = os.path.join(os.path.dirname(__file__), 'data', 'castle.glb')
    with open(output_path, 'wb') as f:
        f.write(glb_data)
    print(f"Written {len(glb_data)} bytes to {output_path}")


if __name__ == '__main__':
    main()
