#!/usr/bin/env python3
"""
Generate test images for Canny edge detection benchmarks.
Creates grayscale images with edges (gradients, shapes, etc.)
"""

import os
import random
import math

def create_gradient_image(h, w, filename):
    """Horizontal gradient - good for testing edge detection"""
    os.makedirs(os.path.dirname(filename) if os.path.dirname(filename) else ".", exist_ok=True)
    
    with open(filename, 'w') as f:
        f.write(f"{h} {w} 1\n")
        for y in range(h):
            row = []
            for x in range(w):
                # Horizontal gradient with some vertical stripes
                val = int((x / w) * 255)
                if x % 50 < 5:  # Add vertical edges
                    val = 255 - val
                row.append(str(val))
            f.write(" ".join(row) + "\n")
    
    print(f"Created {filename}: {w}x{h}")

def create_shapes_image(h, w, filename):
    """Image with geometric shapes - rectangles and circles"""
    os.makedirs(os.path.dirname(filename) if os.path.dirname(filename) else ".", exist_ok=True)
    
    pixels = [[50 for _ in range(w)] for _ in range(h)]  # Dark background
    
    # Add some rectangles
    for _ in range(5):
        rx = random.randint(0, w - 100)
        ry = random.randint(0, h - 100)
        rw = random.randint(30, 100)
        rh = random.randint(30, 100)
        val = random.randint(150, 250)
        for y in range(ry, min(ry + rh, h)):
            for x in range(rx, min(rx + rw, w)):
                pixels[y][x] = val
    
    # Add some circles
    for _ in range(3):
        cx = random.randint(50, w - 50)
        cy = random.randint(50, h - 50)
        r = random.randint(20, 50)
        val = random.randint(150, 250)
        for y in range(max(0, cy - r), min(h, cy + r + 1)):
            for x in range(max(0, cx - r), min(w, cx + r + 1)):
                if (x - cx) ** 2 + (y - cy) ** 2 <= r ** 2:
                    pixels[y][x] = val
    
    with open(filename, 'w') as f:
        f.write(f"{h} {w} 1\n")
        for row in pixels:
            f.write(" ".join(map(str, row)) + "\n")
    
    print(f"Created {filename}: {w}x{h}")

def create_checkerboard_image(h, w, filename, block_size=32):
    """Checkerboard pattern - lots of edges"""
    os.makedirs(os.path.dirname(filename) if os.path.dirname(filename) else ".", exist_ok=True)
    
    with open(filename, 'w') as f:
        f.write(f"{h} {w} 1\n")
        for y in range(h):
            row = []
            for x in range(w):
                bx = x // block_size
                by = y // block_size
                val = 200 if (bx + by) % 2 == 0 else 50
                row.append(str(val))
            f.write(" ".join(row) + "\n")
    
    print(f"Created {filename}: {w}x{h}")

def create_noisy_edges_image(h, w, filename):
    """Image with edges and some noise"""
    os.makedirs(os.path.dirname(filename) if os.path.dirname(filename) else ".", exist_ok=True)
    
    with open(filename, 'w') as f:
        f.write(f"{h} {w} 1\n")
        for y in range(h):
            row = []
            for x in range(w):
                # Base: horizontal bands
                band = (y // 64) % 2
                base = 180 if band == 0 else 60
                
                # Add vertical edges
                if x % 100 < 10:
                    base = 255 - base
                
                # Add noise
                noise = random.randint(-20, 20)
                val = max(0, min(255, base + noise))
                row.append(str(val))
            f.write(" ".join(row) + "\n")
    
    print(f"Created {filename}: {w}x{h}")

if __name__ == "__main__":
    os.makedirs("test_images", exist_ok=True)
    
    # Small test images
    create_gradient_image(256, 256, "test_images/test_gradient_256.txt")
    create_shapes_image(256, 256, "test_images/test_shapes_256.txt")
    create_checkerboard_image(256, 256, "test_images/test_checker_256.txt")
    
    # Medium test images
    create_gradient_image(1024, 1024, "test_images/test_1024x1024.txt")
    create_shapes_image(1024, 1024, "test_images/test_shapes_1024.txt")
    
    # Large test images for benchmarking
    create_noisy_edges_image(2048, 2048, "test_images/test_2048x2048.txt")
    create_noisy_edges_image(4096, 4096, "test_images/test_4096x4096.txt")
    
    # Very large for scaling experiments
    print("\nGenerating large images (may take a moment)...")
    create_noisy_edges_image(8192, 8192, "test_images/test_8192x8192.txt")
    
    print("\nDone! Test images created in test_images/")

