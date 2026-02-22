#!/usr/bin/env python3
"""Generate an impressive medieval castle as a glTF binary (.glb) file
with procedural stone, rock, and roof textures.

Inspired by multi-layered hilltop castles with outer/inner walls,
many towers with conical roofs, a central keep, and inner buildings.
"""

import struct
import json
import math
import random
import io
import os
from PIL import Image, ImageDraw

# --- Constants ---
TEXTURE_SCALE = 1.0 / 4.0  # one texture repeat every 4 meters

# glTF constants
FLOAT = 5126
UNSIGNED_SHORT = 5123
UNSIGNED_INT = 5125
ARRAY_BUFFER = 34962
ELEMENT_ARRAY_BUFFER = 34963
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
        s = {"magFilter": mag_filter, "minFilter": min_filter,
             "wrapS": wrap_s, "wrapT": wrap_t}
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
        positions = [float(v) for v in positions]
        normals = [float(v) for v in normals]
        uvs = [float(v) for v in uvs]
        indices = [int(v) for v in indices]
        pos_data = struct.pack('<' + 'f' * len(positions), *positions)
        pos_bv = self.add_buffer_view(pos_data, ARRAY_BUFFER)
        n_verts = len(positions) // 3
        xs, ys, zs = positions[0::3], positions[1::3], positions[2::3]
        pos_acc = self.add_accessor(
            pos_bv, FLOAT, n_verts, "VEC3",
            [min(xs), min(ys), min(zs)], [max(xs), max(ys), max(zs)])

        norm_data = struct.pack('<' + 'f' * len(normals), *normals)
        norm_bv = self.add_buffer_view(norm_data, ARRAY_BUFFER)
        norm_acc = self.add_accessor(norm_bv, FLOAT, n_verts, "VEC3")

        uv_data = struct.pack('<' + 'f' * len(uvs), *uvs)
        uv_bv = self.add_buffer_view(uv_data, ARRAY_BUFFER)
        uv_acc = self.add_accessor(uv_bv, FLOAT, n_verts, "VEC2")

        if max(indices) > 65535:
            idx_data = struct.pack('<' + 'I' * len(indices), *indices)
            comp_type = UNSIGNED_INT
        else:
            idx_data = struct.pack('<' + 'H' * len(indices), *indices)
            comp_type = UNSIGNED_SHORT
        idx_bv = self.add_buffer_view(idx_data, ELEMENT_ARRAY_BUFFER)
        idx_acc = self.add_accessor(idx_bv, comp_type, len(indices), "SCALAR")

        mesh = {"primitives": [{"attributes": {
            "POSITION": pos_acc, "NORMAL": norm_acc, "TEXCOORD_0": uv_acc,
        }, "indices": idx_acc, "material": material_index}]}
        idx = len(self.meshes)
        self.meshes.append(mesh)
        return idx

    def add_node(self, mesh_index=None, name=None, translation=None, children=None):
        node = {}
        if mesh_index is not None:
            node["mesh"] = mesh_index
        if name:
            node["name"] = name
        if translation:
            node["translation"] = translation
        if children:
            node["children"] = children
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
        json_pad = (4 - len(json_bytes) % 4) % 4
        json_bytes += b' ' * json_pad

        bin_data = bytes(self.buffer)
        bin_pad = (4 - len(bin_data) % 4) % 4
        bin_data += b'\x00' * bin_pad

        total_length = 12 + 8 + len(json_bytes) + 8 + len(bin_data)
        out = bytearray()
        out.extend(struct.pack('<III', 0x46546C67, 2, total_length))
        out.extend(struct.pack('<II', len(json_bytes), 0x4E4F534A))
        out.extend(json_bytes)
        out.extend(struct.pack('<II', len(bin_data), 0x004E4942))
        out.extend(bin_data)
        return bytes(out)


# ============================================================
# Geometry Generators
# ============================================================

def generate_box(width, height, depth, pos=(0, 0, 0)):
    """Box with 24 vertices, per-face normals, world-space UVs."""
    hw, hh, hd = width / 2, height / 2, depth / 2
    px, py, pz = pos
    positions, normals, uvs, indices = [], [], [], []

    faces = [
        ((0, 0, 1), [(-hw, -hh, hd), (hw, -hh, hd), (hw, hh, hd), (-hw, hh, hd)], (0, 1)),
        ((0, 0, -1), [(hw, -hh, -hd), (-hw, -hh, -hd), (-hw, hh, -hd), (hw, hh, -hd)], (0, 1)),
        ((1, 0, 0), [(hw, -hh, hd), (hw, -hh, -hd), (hw, hh, -hd), (hw, hh, hd)], (2, 1)),
        ((-1, 0, 0), [(-hw, -hh, -hd), (-hw, -hh, hd), (-hw, hh, hd), (-hw, hh, -hd)], (2, 1)),
        ((0, 1, 0), [(-hw, hh, hd), (hw, hh, hd), (hw, hh, -hd), (-hw, hh, -hd)], (0, 2)),
        ((0, -1, 0), [(-hw, -hh, -hd), (hw, -hh, -hd), (hw, -hh, hd), (-hw, -hh, hd)], (0, 2)),
    ]

    base_idx = 0
    for normal, corners, (u_axis, v_axis) in faces:
        for corner in corners:
            wx, wy, wz = corner[0] + px, corner[1] + py, corner[2] + pz
            positions.extend([wx, wy, wz])
            normals.extend(normal)
            uvs.extend([([wx, wy, wz])[u_axis] * TEXTURE_SCALE,
                        ([wx, wy, wz])[v_axis] * TEXTURE_SCALE])
        indices.extend([base_idx, base_idx + 1, base_idx + 2,
                        base_idx, base_idx + 2, base_idx + 3])
        base_idx += 4
    return positions, normals, uvs, indices


def generate_cylinder(radius, height, segments, pos=(0, 0, 0)):
    """Cylinder with CCW winding, per-face normals, world-space UVs."""
    px, py, pz = pos
    positions, normals, uvs, indices = [], [], [], []
    base_idx = 0
    circ = 2 * math.pi * radius

    # Sides
    for i in range(segments):
        a0 = 2 * math.pi * i / segments
        a1 = 2 * math.pi * (i + 1) / segments
        c0, s0 = math.cos(a0), math.sin(a0)
        c1, s1 = math.cos(a1), math.sin(a1)
        verts = [(radius*c1, 0, radius*s1), (radius*c0, 0, radius*s0),
                 (radius*c0, height, radius*s0), (radius*c1, height, radius*s1)]
        nrms = [(c1, 0, s1), (c0, 0, s0), (c0, 0, s0), (c1, 0, s1)]
        for vi, v in enumerate(verts):
            positions.extend([v[0]+px, v[1]+py, v[2]+pz])
            normals.extend(nrms[vi])
            u = (circ * ((i+1) if vi in (0, 3) else i) / segments) * TEXTURE_SCALE
            uvs.extend([u, (v[1]+py) * TEXTURE_SCALE])
        indices.extend([base_idx, base_idx+1, base_idx+2,
                        base_idx, base_idx+2, base_idx+3])
        base_idx += 4

    # Top cap
    ci = base_idx
    positions.extend([px, py+height, pz]); normals.extend([0, 1, 0]); uvs.extend([0.5, 0.5])
    base_idx += 1
    for i in range(segments):
        a = 2*math.pi*i/segments
        positions.extend([radius*math.cos(a)+px, py+height, radius*math.sin(a)+pz])
        normals.extend([0, 1, 0])
        uvs.extend([0.5+0.5*math.cos(a), 0.5+0.5*math.sin(a)])
        base_idx += 1
    for i in range(segments):
        indices.extend([ci, ci+1+i, ci+1+(i+1) % segments])

    # Bottom cap
    ci = base_idx
    positions.extend([px, py, pz]); normals.extend([0, -1, 0]); uvs.extend([0.5, 0.5])
    base_idx += 1
    for i in range(segments):
        a = 2*math.pi*i/segments
        positions.extend([radius*math.cos(a)+px, py, radius*math.sin(a)+pz])
        normals.extend([0, -1, 0])
        uvs.extend([0.5+0.5*math.cos(a), 0.5+0.5*math.sin(a)])
        base_idx += 1
    for i in range(segments):
        indices.extend([ci, ci+1+(i+1) % segments, ci+1+i])

    return positions, normals, uvs, indices


def generate_cone(radius, height, segments, pos=(0, 0, 0)):
    """Cone (for tower roofs). Apex at top, base at pos.y."""
    px, py, pz = pos
    positions, normals, uvs, indices = [], [], [], []
    base_idx = 0

    # The slope angle for computing normals
    slope = radius / math.sqrt(radius*radius + height*height)
    ny = radius / math.sqrt(radius*radius + height*height)
    nr = height / math.sqrt(radius*radius + height*height)

    # Side triangles
    for i in range(segments):
        a0 = 2*math.pi*i/segments
        a1 = 2*math.pi*(i+1)/segments
        c0, s0 = math.cos(a0), math.sin(a0)
        c1, s1 = math.cos(a1), math.sin(a1)
        cm, sm = math.cos((a0+a1)/2), math.sin((a0+a1)/2)

        # Three vertices per triangle: base0, base1, apex
        positions.extend([radius*c1+px, py, radius*s1+pz])
        normals.extend([nr*c1, ny, nr*s1])
        uvs.extend([(i+1)/segments, 0])

        positions.extend([radius*c0+px, py, radius*s0+pz])
        normals.extend([nr*c0, ny, nr*s0])
        uvs.extend([i/segments, 0])

        positions.extend([px, py+height, pz])
        normals.extend([nr*cm, ny, nr*sm])
        uvs.extend([(i+0.5)/segments, 1])

        indices.extend([base_idx, base_idx+1, base_idx+2])
        base_idx += 3

    # Bottom cap
    ci = base_idx
    positions.extend([px, py, pz]); normals.extend([0, -1, 0]); uvs.extend([0.5, 0.5])
    base_idx += 1
    for i in range(segments):
        a = 2*math.pi*i/segments
        positions.extend([radius*math.cos(a)+px, py, radius*math.sin(a)+pz])
        normals.extend([0, -1, 0])
        uvs.extend([0.5+0.5*math.cos(a), 0.5+0.5*math.sin(a)])
        base_idx += 1
    for i in range(segments):
        indices.extend([ci, ci+1+(i+1) % segments, ci+1+i])

    return positions, normals, uvs, indices


def generate_wedge_roof(width, depth, peak_height, pos=(0, 0, 0)):
    """A triangular prism roof (peaked along X axis, ridge runs along Z)."""
    hw, hd = width / 2, depth / 2
    px, py, pz = pos
    positions, normals, uvs, indices = [], [], [], []
    base_idx = 0

    # Slope angle
    slope_len = math.sqrt(hw*hw + peak_height*peak_height)
    snx = peak_height / slope_len
    sny = hw / slope_len

    # Left slope (looking from +Z)
    for v in [(-hw, 0, -hd), (-hw, 0, hd), (0, peak_height, hd), (0, peak_height, -hd)]:
        positions.extend([v[0]+px, v[1]+py, v[2]+pz])
        normals.extend([-snx, sny, 0])
        uvs.extend([(v[2]+pz)*TEXTURE_SCALE, (v[1]+py)*TEXTURE_SCALE])
    indices.extend([base_idx, base_idx+1, base_idx+2, base_idx, base_idx+2, base_idx+3])
    base_idx += 4

    # Right slope
    for v in [(hw, 0, hd), (hw, 0, -hd), (0, peak_height, -hd), (0, peak_height, hd)]:
        positions.extend([v[0]+px, v[1]+py, v[2]+pz])
        normals.extend([snx, sny, 0])
        uvs.extend([(v[2]+pz)*TEXTURE_SCALE, (v[1]+py)*TEXTURE_SCALE])
    indices.extend([base_idx, base_idx+1, base_idx+2, base_idx, base_idx+2, base_idx+3])
    base_idx += 4

    # Front gable (+Z)
    positions.extend([hw+px, py, hd+pz, -hw+px, py, hd+pz, px, peak_height+py, hd+pz])
    normals.extend([0, 0, 1]*3)
    uvs.extend([(hw+px)*TEXTURE_SCALE, py*TEXTURE_SCALE,
                (-hw+px)*TEXTURE_SCALE, py*TEXTURE_SCALE,
                px*TEXTURE_SCALE, (peak_height+py)*TEXTURE_SCALE])
    indices.extend([base_idx, base_idx+1, base_idx+2])
    base_idx += 3

    # Back gable (-Z)
    positions.extend([-hw+px, py, -hd+pz, hw+px, py, -hd+pz, px, peak_height+py, -hd+pz])
    normals.extend([0, 0, -1]*3)
    uvs.extend([(-hw+px)*TEXTURE_SCALE, py*TEXTURE_SCALE,
                (hw+px)*TEXTURE_SCALE, py*TEXTURE_SCALE,
                px*TEXTURE_SCALE, (peak_height+py)*TEXTURE_SCALE])
    indices.extend([base_idx, base_idx+1, base_idx+2])
    base_idx += 3

    # Bottom
    for v in [(-hw, 0, -hd), (hw, 0, -hd), (hw, 0, hd), (-hw, 0, hd)]:
        positions.extend([v[0]+px, v[1]+py, v[2]+pz])
        normals.extend([0, -1, 0])
        uvs.extend([(v[0]+px)*TEXTURE_SCALE, (v[2]+pz)*TEXTURE_SCALE])
    indices.extend([base_idx, base_idx+1, base_idx+2, base_idx, base_idx+2, base_idx+3])
    base_idx += 4

    return positions, normals, uvs, indices


def merge_geometry(*geom_list):
    """Merge multiple geometry tuples into one."""
    all_pos, all_norm, all_uv, all_idx = [], [], [], []
    voff = 0
    for pos, norm, uv, idx in geom_list:
        all_pos.extend(pos)
        all_norm.extend(norm)
        all_uv.extend(uv)
        all_idx.extend(i + voff for i in idx)
        voff += len(pos) // 3
    return all_pos, all_norm, all_uv, all_idx


# ============================================================
# Texture Generators
# ============================================================

def generate_stone_texture(width=512, height=512):
    """Procedural stone wall texture with running bond pattern."""
    random.seed(42)
    img = Image.new('RGB', (width, height))
    pixels = img.load()

    mortar = (120, 110, 100)
    for y in range(height):
        for x in range(width):
            pixels[x, y] = tuple(c + random.randint(-5, 5) for c in mortar)

    block_w, block_h, gap = 64, 32, 3
    palettes = [(190, 180, 160, 20), (165, 160, 155, 15),
                (140, 135, 130, 15), (155, 175, 145, 12)]
    weights = [0.40, 0.35, 0.15, 0.10]

    for row in range(height // block_h):
        offset = (block_w // 2) if (row % 2 == 1) else 0
        for col in range(width // block_w + 1):
            r = random.random()
            cum = 0
            pal = palettes[0]
            for p, w in zip(palettes, weights):
                cum += w
                if r <= cum:
                    pal = p
                    break
            br, bg, bb, var = pal
            x0, y0 = col * block_w + offset, row * block_h
            x1, y1 = x0 + block_w - gap, y0 + block_h - gap
            for sy in range(max(0, y0), min(height, y1)):
                for sx in range(max(0, x0), min(width, x1)):
                    ax = sx % width
                    pixels[ax, sy] = (max(0, min(255, br + random.randint(-var, var))),
                                      max(0, min(255, bg + random.randint(-var, var))),
                                      max(0, min(255, bb + random.randint(-var, var))))
    return img


def generate_rock_texture(width=512, height=512):
    """Procedural rough rock/cliff texture."""
    random.seed(99)
    img = Image.new('RGB', (width, height))
    pixels = img.load()

    for y in range(height):
        for x in range(width):
            # Base brownish-gray rock
            base = 110 + random.randint(-20, 20)
            r = min(255, max(0, base + 20 + random.randint(-15, 15)))
            g = min(255, max(0, base + 5 + random.randint(-15, 15)))
            b = min(255, max(0, base - 10 + random.randint(-15, 15)))
            pixels[x, y] = (r, g, b)

    # Add some horizontal crack lines
    for _ in range(30):
        y = random.randint(0, height - 1)
        x_start = random.randint(0, width // 2)
        length = random.randint(20, 150)
        darkness = random.randint(30, 60)
        for dx in range(length):
            x = (x_start + dx) % width
            yr = y + random.randint(-1, 1)
            if 0 <= yr < height:
                pr, pg, pb = pixels[x, yr]
                pixels[x, yr] = (max(0, pr - darkness), max(0, pg - darkness),
                                  max(0, pb - darkness))

    # Add some lighter patches (mineral deposits)
    for _ in range(20):
        cx, cy = random.randint(0, width-1), random.randint(0, height-1)
        rad = random.randint(5, 20)
        for dy in range(-rad, rad+1):
            for dx in range(-rad, rad+1):
                if dx*dx + dy*dy <= rad*rad:
                    px, py2 = (cx+dx) % width, (cy+dy) % height
                    pr, pg, pb = pixels[px, py2]
                    pixels[px, py2] = (min(255, pr+15), min(255, pg+15), min(255, pb+10))
    return img


def generate_roof_texture(width=512, height=512):
    """Procedural dark roof tile texture."""
    random.seed(77)
    img = Image.new('RGB', (width, height))
    pixels = img.load()

    # Base dark color
    for y in range(height):
        for x in range(width):
            pixels[x, y] = (60 + random.randint(-5, 5),
                            35 + random.randint(-5, 5),
                            30 + random.randint(-5, 5))

    # Tile rows
    tile_h = 24
    tile_w = 48
    for row in range(height // tile_h + 1):
        offset = (tile_w // 2) if (row % 2 == 1) else 0
        y0 = row * tile_h
        for col in range(width // tile_w + 2):
            x0 = col * tile_w + offset
            # Each tile slightly different shade
            shade = random.randint(-10, 10)
            tr = max(0, min(255, 75 + shade + random.randint(-5, 5)))
            tg = max(0, min(255, 42 + shade + random.randint(-5, 5)))
            tb = max(0, min(255, 35 + shade + random.randint(-5, 5)))
            for sy in range(max(0, y0 + 2), min(height, y0 + tile_h - 1)):
                for sx in range(max(0, x0 + 1), min(width, x0 + tile_w - 1)):
                    ax = sx % width
                    pixels[ax, sy] = (max(0, min(255, tr + random.randint(-3, 3))),
                                      max(0, min(255, tg + random.randint(-3, 3))),
                                      max(0, min(255, tb + random.randint(-3, 3))))
    return img


# ============================================================
# Castle Builder
# ============================================================

def add_tower(parts, x, z, base_y, radius, height, segments=16):
    """Add a cylindrical tower with crenellations. Returns roof parts separately."""
    parts['stone'].append(generate_cylinder(radius, height, segments, (x, base_y, z)))
    # Crenellations
    n_merlons = max(6, int(radius * 3))
    for ci in range(n_merlons):
        angle = 2 * math.pi * ci / n_merlons
        mx = x + (radius - 0.3) * math.cos(angle)
        mz = z + (radius - 0.3) * math.sin(angle)
        merlon_size = min(0.8, radius * 0.3)
        parts['stone'].append(generate_box(
            merlon_size, 1.2, merlon_size,
            (mx, base_y + height + 0.6, mz)))


def add_tower_roof(parts, x, z, base_y, radius, roof_height, segments=16):
    """Add a conical roof for a tower."""
    parts['roof'].append(generate_cone(
        radius * 1.15, roof_height, segments, (x, base_y, z)))


def add_wall(parts, x1, z1, x2, z2, base_y, height, thickness):
    """Add a wall segment between two points with merlons on top."""
    dx, dz = x2 - x1, z2 - z1
    length = math.sqrt(dx*dx + dz*dz)
    cx, cz = (x1 + x2) / 2, (z1 + z2) / 2

    # Determine wall orientation
    if abs(dx) > abs(dz):
        # Runs along X
        parts['stone'].append(generate_box(length, height, thickness,
                                           (cx, base_y + height/2, cz)))
        # Merlons
        n_merlons = int(length / 2.5)
        for i in range(n_merlons):
            mx = x1 + (i + 0.5) * length / n_merlons
            parts['stone'].append(generate_box(
                0.8, 1.2, thickness,
                (mx, base_y + height + 0.6, cz)))
    else:
        # Runs along Z
        parts['stone'].append(generate_box(thickness, height, length,
                                           (cx, base_y + height/2, cz)))
        n_merlons = int(length / 2.5)
        for i in range(n_merlons):
            mz = z1 + (i + 0.5) * length / n_merlons
            parts['stone'].append(generate_box(
                thickness, 1.2, 0.8,
                (cx, base_y + height + 0.6, mz)))


def add_building(parts, x, z, base_y, w, h, d, roof_h):
    """Add a rectangular building with walls and a peaked roof."""
    parts['stone'].append(generate_box(w, h, d, (x, base_y + h/2, z)))
    parts['roof'].append(generate_wedge_roof(w, d, roof_h, (x, base_y + h, z)))


def generate_terrain_mesh(size=500, resolution=120, castle_radius=40):
    """Generate a terrain heightmap mesh with varied hills, flat near the castle."""
    rng = random.Random(123)
    half = size / 2
    step = size / resolution

    # Generate heightmap
    heights = [[0.0] * (resolution + 1) for _ in range(resolution + 1)]

    # Large rolling hills
    for _ in range(25):
        cx = rng.uniform(-half, half)
        cz = rng.uniform(-half, half)
        bump_r = rng.uniform(50, 120)
        bump_h = rng.uniform(3, 12)
        for gz in range(resolution + 1):
            for gx in range(resolution + 1):
                wx = -half + gx * step
                wz = -half + gz * step
                dx, dz = wx - cx, wz - cz
                dist2 = dx*dx + dz*dz
                if dist2 < bump_r * bump_r:
                    falloff = 1.0 - dist2 / (bump_r * bump_r)
                    heights[gz][gx] += bump_h * falloff * falloff

    # Medium undulations
    for _ in range(60):
        cx = rng.uniform(-half, half)
        cz = rng.uniform(-half, half)
        bump_r = rng.uniform(15, 40)
        bump_h = rng.uniform(1, 4)
        for gz in range(resolution + 1):
            for gx in range(resolution + 1):
                wx = -half + gx * step
                wz = -half + gz * step
                dx, dz = wx - cx, wz - cz
                dist2 = dx*dx + dz*dz
                if dist2 < bump_r * bump_r:
                    falloff = 1.0 - dist2 / (bump_r * bump_r)
                    heights[gz][gx] += bump_h * falloff * falloff

    # Small bumps for micro-detail
    for _ in range(150):
        cx = rng.uniform(-half, half)
        cz = rng.uniform(-half, half)
        bump_r = rng.uniform(5, 15)
        bump_h = rng.uniform(0.3, 1.5)
        for gz in range(resolution + 1):
            for gx in range(resolution + 1):
                wx = -half + gx * step
                wz = -half + gz * step
                dx, dz = wx - cx, wz - cz
                dist2 = dx*dx + dz*dz
                if dist2 < bump_r * bump_r:
                    falloff = 1.0 - dist2 / (bump_r * bump_r)
                    heights[gz][gx] += bump_h * falloff

    # A few valleys (negative bumps)
    for _ in range(10):
        cx = rng.uniform(-half * 0.8, half * 0.8)
        cz = rng.uniform(-half * 0.8, half * 0.8)
        bump_r = rng.uniform(20, 50)
        bump_h = rng.uniform(1, 3)
        for gz in range(resolution + 1):
            for gx in range(resolution + 1):
                wx = -half + gx * step
                wz = -half + gz * step
                dx, dz = wx - cx, wz - cz
                dist2 = dx*dx + dz*dz
                if dist2 < bump_r * bump_r:
                    falloff = 1.0 - dist2 / (bump_r * bump_r)
                    heights[gz][gx] -= bump_h * falloff * falloff

    # Flatten near the castle center and the approach path
    for gz in range(resolution + 1):
        for gx in range(resolution + 1):
            wx = -half + gx * step
            wz = -half + gz * step
            dist = math.sqrt(wx*wx + wz*wz)
            if dist < castle_radius:
                t = dist / castle_radius
                t = max(0, min(1, (t - 0.4) * (1.0 / 0.6)))
                heights[gz][gx] *= t * t
            # Flatten the approach path (south, positive Z)
            if abs(wx) < 8 and wz > 0:
                path_factor = max(0, 1.0 - abs(wx) / 8)
                heights[gz][gx] *= (1.0 - path_factor * 0.85)

    # Clamp minimum height so terrain doesn't go below 0
    for gz in range(resolution + 1):
        for gx in range(resolution + 1):
            heights[gz][gx] = max(-0.5, heights[gz][gx])

    # Smooth pass
    for _ in range(2):
        new_h = [[0.0] * (resolution + 1) for _ in range(resolution + 1)]
        for gz in range(resolution + 1):
            for gx in range(resolution + 1):
                total, count = heights[gz][gx], 1
                for dz2, dx2 in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                    nz, nx = gz + dz2, gx + dx2
                    if 0 <= nz <= resolution and 0 <= nx <= resolution:
                        total += heights[nz][nx]
                        count += 1
                new_h[gz][gx] = total / count
        heights = new_h

    # Build mesh
    positions, normals, uvs, indices = [], [], [], []

    for gz in range(resolution + 1):
        for gx in range(resolution + 1):
            wx = -half + gx * step
            wz = -half + gz * step
            wy = heights[gz][gx]
            positions.extend([wx, wy, wz])
            uvs.extend([wx * TEXTURE_SCALE, wz * TEXTURE_SCALE])

    # Compute normals per vertex (average of adjacent face normals)
    vertex_normals = [[0.0, 0.0, 0.0] for _ in range((resolution+1)*(resolution+1))]
    for gz in range(resolution):
        for gx in range(resolution):
            i00 = gz * (resolution + 1) + gx
            i10 = i00 + 1
            i01 = i00 + (resolution + 1)
            i11 = i01 + 1

            # Triangle 1: i00, i01, i10
            p0 = positions[i00*3:i00*3+3]
            p1 = positions[i01*3:i01*3+3]
            p2 = positions[i10*3:i10*3+3]
            e1 = [p1[j]-p0[j] for j in range(3)]
            e2 = [p2[j]-p0[j] for j in range(3)]
            n = [e1[1]*e2[2]-e1[2]*e2[1], e1[2]*e2[0]-e1[0]*e2[2], e1[0]*e2[1]-e1[1]*e2[0]]
            for vi in [i00, i01, i10]:
                for j in range(3):
                    vertex_normals[vi][j] += n[j]

            # Triangle 2: i10, i01, i11
            p0 = positions[i10*3:i10*3+3]
            p1 = positions[i01*3:i01*3+3]
            p2 = positions[i11*3:i11*3+3]
            e1 = [p1[j]-p0[j] for j in range(3)]
            e2 = [p2[j]-p0[j] for j in range(3)]
            n = [e1[1]*e2[2]-e1[2]*e2[1], e1[2]*e2[0]-e1[0]*e2[2], e1[0]*e2[1]-e1[1]*e2[0]]
            for vi in [i10, i01, i11]:
                for j in range(3):
                    vertex_normals[vi][j] += n[j]

            indices.extend([i00, i01, i10, i10, i01, i11])

    # Normalize
    for vn in vertex_normals:
        length = math.sqrt(vn[0]**2 + vn[1]**2 + vn[2]**2)
        if length > 0:
            vn[0] /= length
            vn[1] /= length
            vn[2] /= length
        else:
            vn[1] = 1.0
    for vn in vertex_normals:
        normals.extend(vn)

    return positions, normals, uvs, indices


def generate_tree(x, z, ground_y, tree_type='oak', rng=None):
    """Generate a realistic tree with trunk and multi-layered canopy.

    Returns (bark_parts, leaves_parts) - lists of geometry tuples.
    tree_type: 'oak' (broad, round), 'pine' (tall, conical), 'birch' (slim, tall)
    """
    if rng is None:
        rng = random.Random()
    bark_parts = []
    leaves_parts = []

    if tree_type == 'oak':
        # Thick trunk, slightly tapered
        trunk_h = rng.uniform(3.5, 5.5)
        trunk_r_base = rng.uniform(0.35, 0.55)
        trunk_r_top = trunk_r_base * 0.7
        # Main trunk
        bark_parts.append(generate_cylinder(trunk_r_base, trunk_h, 8,
                                             (x, ground_y, z)))
        # Branch stumps (thicker cylinders angled out)
        for bi in range(rng.randint(2, 4)):
            ba = rng.uniform(0, 2 * math.pi)
            bh_start = trunk_h * rng.uniform(0.5, 0.85)
            br = trunk_r_base * 0.5
            bl = rng.uniform(1.0, 2.0)
            bx = x + math.cos(ba) * bl * 0.5
            bz = z + math.sin(ba) * bl * 0.5
            bark_parts.append(generate_cylinder(br, bl, 6,
                (bx, ground_y + bh_start, bz)))

        # Multi-layered canopy (overlapping ellipsoid-like shapes using
        # flattened cones and inverted cones to approximate round canopy)
        canopy_base = ground_y + trunk_h * 0.6
        canopy_top = ground_y + trunk_h + rng.uniform(3, 5)
        canopy_mid = (canopy_base + canopy_top) / 2
        canopy_r = rng.uniform(2.5, 4.0)

        # Main large canopy dome
        leaves_parts.append(generate_cone(canopy_r, canopy_top - canopy_mid, 12,
                                           (x, canopy_mid, z)))
        # Lower skirt (inverted wider cone)
        leaves_parts.append(generate_cone(canopy_r * 1.1, canopy_mid - canopy_base, 12,
                                           (x, canopy_base, z)))
        # Several offset sub-canopy blobs
        for _ in range(rng.randint(3, 6)):
            off_a = rng.uniform(0, 2 * math.pi)
            off_d = rng.uniform(0.5, canopy_r * 0.6)
            off_y = rng.uniform(canopy_mid - 1, canopy_mid + 1)
            off_r = rng.uniform(1.2, canopy_r * 0.7)
            off_h = rng.uniform(1.5, 3.0)
            leaves_parts.append(generate_cone(off_r, off_h, 10,
                (x + math.cos(off_a) * off_d, off_y, z + math.sin(off_a) * off_d)))

    elif tree_type == 'pine':
        # Tall straight trunk
        trunk_h = rng.uniform(6, 10)
        trunk_r = rng.uniform(0.2, 0.35)
        bark_parts.append(generate_cylinder(trunk_r, trunk_h, 8,
                                             (x, ground_y, z)))
        # Layered conical canopy (3-5 tiers, progressively smaller)
        n_tiers = rng.randint(3, 5)
        for ti in range(n_tiers):
            t = ti / n_tiers
            tier_y = ground_y + trunk_h * (0.3 + t * 0.6)
            tier_r = rng.uniform(1.5, 3.0) * (1.0 - t * 0.6)
            tier_h = rng.uniform(2.0, 3.5) * (1.0 - t * 0.3)
            # Slight random offset for natural look
            ox = rng.uniform(-0.3, 0.3)
            oz = rng.uniform(-0.3, 0.3)
            leaves_parts.append(generate_cone(tier_r, tier_h, 10,
                                               (x + ox, tier_y, z + oz)))
        # Top spire
        leaves_parts.append(generate_cone(0.8, 2.0, 8,
            (x, ground_y + trunk_h, z)))

    elif tree_type == 'birch':
        # Slim tall trunk, white-ish bark (handled by texture)
        trunk_h = rng.uniform(5, 8)
        trunk_r = rng.uniform(0.15, 0.25)
        bark_parts.append(generate_cylinder(trunk_r, trunk_h, 8,
                                             (x, ground_y, z)))
        # Light, airy canopy - several small cone clusters
        canopy_base_y = ground_y + trunk_h * 0.5
        for _ in range(rng.randint(5, 8)):
            off_a = rng.uniform(0, 2 * math.pi)
            off_d = rng.uniform(0.3, 1.5)
            off_y = rng.uniform(canopy_base_y, ground_y + trunk_h + 1)
            cr = rng.uniform(0.8, 1.8)
            ch = rng.uniform(1.0, 2.5)
            leaves_parts.append(generate_cone(cr, ch, 8,
                (x + math.cos(off_a) * off_d, off_y, z + math.sin(off_a) * off_d)))

    return bark_parts, leaves_parts


def generate_grass_texture(width=512, height=512):
    """Rich procedural grass texture with blade detail and variation."""
    rng = random.Random(55)
    img = Image.new('RGB', (width, height))
    pixels = img.load()

    # Base layer: varied green with earthy undertone
    for y in range(height):
        for x in range(width):
            # Mix of greens
            base_g = 75 + rng.randint(-10, 10)
            r = max(0, min(255, 45 + rng.randint(-12, 12)))
            g = max(0, min(255, base_g + rng.randint(-8, 8)))
            b = max(0, min(255, 28 + rng.randint(-8, 8)))
            pixels[x, y] = (r, g, b)

    # Grass blade streaks (vertical-ish bright lines)
    for _ in range(800):
        bx = rng.randint(0, width - 1)
        by = rng.randint(0, height - 1)
        blade_len = rng.randint(3, 12)
        bright = rng.randint(10, 35)
        dx_drift = rng.choice([-1, 0, 0, 0, 1])
        for step in range(blade_len):
            py2 = (by + step) % height
            px2 = (bx + dx_drift * step // 3) % width
            pr, pg, pb = pixels[px2, py2]
            pixels[px2, py2] = (min(255, pr + bright // 3),
                                min(255, pg + bright),
                                min(255, pb + bright // 4))

    # Darker grass clumps
    for _ in range(40):
        cx, cy = rng.randint(0, width-1), rng.randint(0, height-1)
        rad = rng.randint(8, 25)
        darkness = rng.randint(10, 25)
        for dy in range(-rad, rad+1):
            for dx in range(-rad, rad+1):
                if dx*dx + dy*dy <= rad*rad:
                    px2, py2 = (cx+dx) % width, (cy+dy) % height
                    pr, pg, pb = pixels[px2, py2]
                    dist_f = (dx*dx + dy*dy) / (rad*rad)
                    d = int(darkness * (1 - dist_f))
                    pixels[px2, py2] = (max(0, pr - d),
                                        min(255, pg + d // 2),
                                        max(0, pb - d // 2))

    # Lighter sun-hit patches
    for _ in range(25):
        cx, cy = rng.randint(0, width-1), rng.randint(0, height-1)
        rad = rng.randint(12, 35)
        for dy in range(-rad, rad+1):
            for dx in range(-rad, rad+1):
                if dx*dx + dy*dy <= rad*rad:
                    px2, py2 = (cx+dx) % width, (cy+dy) % height
                    pr, pg, pb = pixels[px2, py2]
                    dist_f = (dx*dx + dy*dy) / (rad*rad)
                    lift = int(15 * (1 - dist_f))
                    pixels[px2, py2] = (min(255, pr + lift),
                                        min(255, pg + lift + 5),
                                        min(255, pb + lift // 2))

    # Dirt patches
    for _ in range(12):
        cx, cy = rng.randint(0, width-1), rng.randint(0, height-1)
        rad = rng.randint(6, 18)
        for dy in range(-rad, rad+1):
            for dx in range(-rad, rad+1):
                if dx*dx + dy*dy <= rad*rad:
                    px2, py2 = (cx+dx) % width, (cy+dy) % height
                    pr, pg, pb = pixels[px2, py2]
                    dist_f = (dx*dx + dy*dy) / (rad*rad)
                    f = 1 - dist_f
                    pixels[px2, py2] = (min(255, int(pr + 35 * f)),
                                        max(0, int(pg - 20 * f)),
                                        max(0, int(pb + 5 * f)))

    # Tiny yellow/dry grass specks
    for _ in range(300):
        px2 = rng.randint(0, width-1)
        py2 = rng.randint(0, height-1)
        pixels[px2, py2] = (rng.randint(120, 160), rng.randint(110, 140), rng.randint(40, 70))

    return img


def generate_bark_texture(width=256, height=256):
    """Simple brown bark texture."""
    rng = random.Random(33)
    img = Image.new('RGB', (width, height))
    pixels = img.load()
    for y in range(height):
        for x in range(width):
            base = 70 + rng.randint(-10, 10)
            # Vertical streaks
            streak = int(10 * math.sin(x * 0.5 + rng.uniform(-0.5, 0.5)))
            r = max(0, min(255, base + 15 + streak + rng.randint(-5, 5)))
            g = max(0, min(255, base - 5 + streak + rng.randint(-5, 5)))
            b = max(0, min(255, base - 25 + streak + rng.randint(-5, 5)))
            pixels[x, y] = (r, g, b)
    return img


def generate_leaves_texture(width=256, height=256):
    """Rich green foliage texture with depth and variation."""
    rng = random.Random(44)
    img = Image.new('RGB', (width, height))
    pixels = img.load()

    # Base dark green
    for y in range(height):
        for x in range(width):
            r = max(0, min(255, 25 + rng.randint(-10, 10)))
            g = max(0, min(255, 65 + rng.randint(-15, 15)))
            b = max(0, min(255, 18 + rng.randint(-8, 8)))
            pixels[x, y] = (r, g, b)

    # Leaf-shaped bright spots
    for _ in range(200):
        lx = rng.randint(0, width - 1)
        ly = rng.randint(0, height - 1)
        ls = rng.randint(2, 6)
        bright = rng.randint(15, 40)
        for dy in range(-ls, ls + 1):
            for dx in range(-ls, ls + 1):
                if abs(dx) + abs(dy) <= ls:
                    px2 = (lx + dx) % width
                    py2 = (ly + dy) % height
                    pr, pg, pb = pixels[px2, py2]
                    f = 1 - (abs(dx) + abs(dy)) / ls
                    pixels[px2, py2] = (min(255, pr + int(bright * 0.3 * f)),
                                        min(255, pg + int(bright * f)),
                                        min(255, pb + int(bright * 0.2 * f)))

    # Dark shadow patches
    for _ in range(50):
        cx, cy = rng.randint(0, width-1), rng.randint(0, height-1)
        rad = rng.randint(3, 8)
        for dy in range(-rad, rad+1):
            for dx in range(-rad, rad+1):
                if dx*dx + dy*dy <= rad*rad:
                    px2 = (cx + dx) % width
                    py2 = (cy + dy) % height
                    pr, pg, pb = pixels[px2, py2]
                    pixels[px2, py2] = (max(0, pr - 8), max(0, pg - 12), max(0, pb - 5))

    return img


def build_castle():
    """Build the complete multi-layered castle. Returns dict of geometry by material."""
    parts = {'stone': [], 'rock': [], 'roof': [], 'grass': [], 'bark': [], 'leaves': []}

    # ============================================================
    # Layer 1: Outer curtain wall with 9 towers
    # ============================================================
    outer_r = 22  # half-size of outer wall perimeter
    outer_wall_h = 7.0
    outer_wall_thick = 1.2
    outer_base = 0.0  # castle sits on the terrain
    gate_half = 3.5  # half-width of gate opening

    # 9 outer tower positions (4 corners + 5 midpoints, gate flanked)
    outer_towers = [
        (outer_r, outer_r), (-outer_r, outer_r),
        (-outer_r, -outer_r), (outer_r, -outer_r),
        # South: two gate-flanking towers instead of one blocking tower
        (-gate_half - 4, outer_r + 1), (gate_half + 4, outer_r + 1),
        (0, -outer_r - 1),
        (outer_r + 1, 0), (-outer_r - 1, 0),
    ]
    outer_tower_heights = [10, 10, 10, 10, 11, 11, 9, 9, 11]
    outer_tower_radii = [2.5, 2.5, 2.5, 2.5, 2.0, 2.0, 2.2, 2.2, 2.8]

    for (tx, tz), th, tr in zip(outer_towers, outer_tower_heights, outer_tower_radii):
        add_tower(parts, tx, tz, outer_base, tr, th, 16)
        add_tower_roof(parts, tx, tz, outer_base + th, tr, th * 0.4, 16)

    # Outer walls (connecting corner towers, with gaps for gate on south)
    # North wall
    add_wall(parts, -outer_r, -outer_r, outer_r, -outer_r,
             outer_base, outer_wall_h, outer_wall_thick)
    # East wall
    add_wall(parts, outer_r, -outer_r, outer_r, outer_r,
             outer_base, outer_wall_h, outer_wall_thick)
    # West wall
    add_wall(parts, -outer_r, -outer_r, -outer_r, outer_r,
             outer_base, outer_wall_h, outer_wall_thick)
    # South wall - split for gate
    add_wall(parts, -outer_r, outer_r, -gate_half, outer_r,
             outer_base, outer_wall_h, outer_wall_thick)
    add_wall(parts, gate_half, outer_r, outer_r, outer_r,
             outer_base, outer_wall_h, outer_wall_thick)

    # Gatehouse on south wall
    gate_pillar_h = 10.0
    parts['stone'].append(generate_box(2.5, gate_pillar_h, 2.5,
                                        (-gate_half - 0.5, outer_base + gate_pillar_h/2, outer_r)))
    parts['stone'].append(generate_box(2.5, gate_pillar_h, 2.5,
                                        (gate_half + 0.5, outer_base + gate_pillar_h/2, outer_r)))
    parts['stone'].append(generate_box(gate_half*2 + 2.5, 2.0, 2.5,
                                        (0, outer_base + gate_pillar_h - 1.0, outer_r)))
    # Gatehouse merlons
    for i in range(4):
        mx = -3.0 + i * 2.0
        parts['stone'].append(generate_box(0.8, 1.2, 1.5,
                                            (mx, outer_base + gate_pillar_h + 0.6, outer_r)))

    # ============================================================
    # Layer 3: Inner buildings in the outer courtyard
    # ============================================================
    # Barracks
    add_building(parts, -12, 12, outer_base, 8, 5, 6, 3)
    # Stables
    add_building(parts, 12, 12, outer_base, 6, 4, 8, 2.5)
    # Chapel
    add_building(parts, 14, -10, outer_base, 5, 7, 8, 4)
    # Storage
    add_building(parts, -14, -8, outer_base, 7, 4, 5, 2.5)
    # Small houses
    add_building(parts, -10, 0, outer_base, 4, 3.5, 5, 2)
    add_building(parts, 10, 3, outer_base, 5, 3.5, 4, 2)

    # ============================================================
    # Layer 4: Inner curtain wall (higher elevation) with 4 towers
    # ============================================================
    inner_r = 11
    inner_wall_h = 9.0
    inner_wall_thick = 1.5
    inner_base = 0.0  # same ground level

    inner_towers = [
        (inner_r, inner_r), (-inner_r, inner_r),
        (-inner_r, -inner_r), (inner_r, -inner_r),
    ]
    inner_tower_heights = [14, 13, 13, 14]

    for (tx, tz), th in zip(inner_towers, inner_tower_heights):
        add_tower(parts, tx, tz, inner_base, 2.8, th, 16)
        add_tower_roof(parts, tx, tz, inner_base + th, 2.8, th * 0.35, 16)

    # Inner walls
    add_wall(parts, -inner_r, -inner_r, inner_r, -inner_r,
             inner_base, inner_wall_h, inner_wall_thick)
    add_wall(parts, inner_r, -inner_r, inner_r, inner_r,
             inner_base, inner_wall_h, inner_wall_thick)
    add_wall(parts, -inner_r, -inner_r, -inner_r, inner_r,
             inner_base, inner_wall_h, inner_wall_thick)
    # Inner south wall (with opening)
    inner_gate_half = 2.5
    add_wall(parts, -inner_r, inner_r, -inner_gate_half, inner_r,
             inner_base, inner_wall_h, inner_wall_thick)
    add_wall(parts, inner_gate_half, inner_r, inner_r, inner_r,
             inner_base, inner_wall_h, inner_wall_thick)

    # ============================================================
    # Layer 5: Central keep (the donjon)
    # ============================================================
    keep_base = 0.0

    keep_w, keep_h, keep_d = 8, 18, 8
    parts['stone'].append(generate_box(keep_w, keep_h, keep_d,
                                        (0, keep_base + keep_h/2, 0)))
    # Keep battlements
    for side in range(4):
        for i in range(3):
            off = -2.5 + i * 2.5
            if side == 0:
                parts['stone'].append(generate_box(0.8, 1.5, 0.8,
                    (off, keep_base + keep_h + 0.75, keep_d/2)))
            elif side == 1:
                parts['stone'].append(generate_box(0.8, 1.5, 0.8,
                    (off, keep_base + keep_h + 0.75, -keep_d/2)))
            elif side == 2:
                parts['stone'].append(generate_box(0.8, 1.5, 0.8,
                    (keep_w/2, keep_base + keep_h + 0.75, off)))
            else:
                parts['stone'].append(generate_box(0.8, 1.5, 0.8,
                    (-keep_w/2, keep_base + keep_h + 0.75, off)))

    # Main keep tower (tall central turret)
    main_turret_h = 8.0
    parts['stone'].append(generate_cylinder(2.0, main_turret_h, 16,
                                             (0, keep_base + keep_h, 0)))
    # Turret crenellations
    for ci in range(8):
        angle = 2 * math.pi * ci / 8
        parts['stone'].append(generate_box(0.6, 1.0, 0.6,
            (1.7 * math.cos(angle), keep_base + keep_h + main_turret_h + 0.5,
             1.7 * math.sin(angle))))
    # Turret roof
    add_tower_roof(parts, 0, 0, keep_base + keep_h + main_turret_h,
                   2.0, 5.0, 16)

    # ============================================================
    # Layer 6: Additional smaller towers on the keep
    # ============================================================
    keep_corner_offsets = [(3.5, 3.5), (-3.5, 3.5), (-3.5, -3.5), (3.5, -3.5)]
    for cx, cz in keep_corner_offsets:
        th = 5.0
        parts['stone'].append(generate_cylinder(1.2, th, 12,
                                                 (cx, keep_base + keep_h, cz)))
        add_tower_roof(parts, cx, cz, keep_base + keep_h + th, 1.2, 2.5, 12)

    # ============================================================
    # Layer 7: Great hall next to keep
    # ============================================================
    add_building(parts, 0, -7, inner_base, 7, 8, 6, 4)
    add_building(parts, 0, 7, inner_base, 6, 6, 5, 3)

    # ============================================================
    # Layer 8: Terrain (heightmap ground)
    # ============================================================
    parts['grass'].append(generate_terrain_mesh(
        size=500, resolution=80, castle_radius=40))

    # ============================================================
    # Layer 9: Trees scattered around the landscape
    # ============================================================
    tree_rng = random.Random(200)
    tree_types = ['oak', 'pine', 'birch']
    tree_weights = [0.45, 0.35, 0.20]

    # Dense clusters and scattered individuals
    tree_positions = []
    # Random scattered trees
    for _ in range(150):
        tx = tree_rng.uniform(-200, 200)
        tz = tree_rng.uniform(-200, 200)
        tree_positions.append((tx, tz))

    # Add some forest clusters
    for _ in range(8):
        cluster_x = tree_rng.uniform(-180, 180)
        cluster_z = tree_rng.uniform(-180, 180)
        if math.sqrt(cluster_x**2 + cluster_z**2) < 50:
            continue
        n_cluster = tree_rng.randint(8, 15)
        for _ in range(n_cluster):
            tx = cluster_x + tree_rng.gauss(0, 12)
            tz = cluster_z + tree_rng.gauss(0, 12)
            tree_positions.append((tx, tz))

    for tx, tz in tree_positions:
        dist = math.sqrt(tx*tx + tz*tz)
        if dist < 45:
            continue
        if abs(tx) < 8 and 20 < tz < 55:
            continue
        # Pick tree type
        r = tree_rng.random()
        cum = 0
        ttype = 'oak'
        for tt, tw in zip(tree_types, tree_weights):
            cum += tw
            if r <= cum:
                ttype = tt
                break
        bark_parts, leaves_parts = generate_tree(tx, tz, 0, ttype,
                                                  random.Random(tree_rng.randint(0, 999999)))
        parts['bark'].extend(bark_parts)
        parts['leaves'].extend(leaves_parts)

    return parts


def main():
    print("Generating textures...")
    stone_tex = generate_stone_texture()
    rock_tex = generate_rock_texture()
    roof_tex = generate_roof_texture()
    grass_tex = generate_grass_texture()
    bark_tex = generate_bark_texture()
    leaves_tex = generate_leaves_texture()

    print("Building castle geometry...")
    parts = build_castle()

    builder = GltfBuilder()
    sampler_idx = builder.add_sampler()

    # Materials
    stone_img = builder.add_image_from_pil(stone_tex)
    stone_mat = builder.add_material("Stone",
        builder.add_texture(stone_img, sampler_idx), metallic=0.0, roughness=0.85)

    rock_img = builder.add_image_from_pil(rock_tex)
    rock_mat = builder.add_material("Rock",
        builder.add_texture(rock_img, sampler_idx), metallic=0.0, roughness=0.95)

    roof_img = builder.add_image_from_pil(roof_tex)
    roof_mat = builder.add_material("Roof",
        builder.add_texture(roof_img, sampler_idx), metallic=0.0, roughness=0.75)

    grass_img = builder.add_image_from_pil(grass_tex)
    grass_mat = builder.add_material("Grass",
        builder.add_texture(grass_img, sampler_idx), metallic=0.0, roughness=0.95)

    bark_img = builder.add_image_from_pil(bark_tex)
    bark_mat = builder.add_material("Bark",
        builder.add_texture(bark_img, sampler_idx), metallic=0.0, roughness=0.9)

    leaves_img = builder.add_image_from_pil(leaves_tex)
    leaves_mat = builder.add_material("Leaves",
        builder.add_texture(leaves_img, sampler_idx), metallic=0.0, roughness=0.8)

    mat_map = {'stone': stone_mat, 'rock': rock_mat, 'roof': roof_mat,
               'grass': grass_mat, 'bark': bark_mat, 'leaves': leaves_mat}

    for mat_name, geom_list in parts.items():
        if not geom_list:
            continue
        merged = merge_geometry(*geom_list)
        n_verts = len(merged[0]) // 3
        n_tris = len(merged[3]) // 3
        print(f"  {mat_name}: {n_verts} vertices, {n_tris} triangles")
        mesh_idx = builder.add_mesh(*merged, mat_map[mat_name])
        builder.add_node(mesh_idx, name=f"Castle_{mat_name}")

    glb_data = builder.build_glb()
    output_path = os.path.join(os.path.dirname(__file__), 'data', 'castle.glb')
    with open(output_path, 'wb') as f:
        f.write(glb_data)
    print(f"Written {len(glb_data)} bytes to {output_path}")


if __name__ == '__main__':
    main()
