#!/usr/bin/env python3
"""
Generate large test images for scaling experiments.
"""

import os
import random

def create_bimodal_image(h, w, filename):
    """Create bimodal intensity distribution (good for Otsu)"""
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    
    with open(filename, 'w') as f:
        f.write(f"{h} {w} 1\n")
        for i in range(h * w):
            # 50% dark (20-80), 50% bright (180-240)
            if random.random() < 0.5:
                val = random.randint(20, 80)
            else:
                val = random.randint(180, 240)
            f.write(f"{val}\n")
    
    print(f"Created {filename}: {w}x{h} ({h*w:,} pixels)")

if __name__ == "__main__":
    # Standard sizes
    sizes = [
        (256, 256),
        (1024, 1024),
        (2048, 2048),
        (4096, 4096),
        (8192, 8192),    # 67M pixels
        # (16384, 16384),  # 268M pixels - uncomment if you have RAM
    ]
    
    for h, w in sizes:
        filename = f"test_images/test_{w}x{h}.txt"
        create_bimodal_image(h, w, filename)
    
    # Also create the test_bimodal.txt for compatibility
    create_bimodal_image(256, 256, "test_images/test_bimodal.txt")
    
    print("\nDone! Large images created.")
    print("Note: 8192x8192 = 67M pixels, may take a while to process")

