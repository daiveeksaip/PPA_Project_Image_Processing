#!/usr/bin/env python3
"""
generate_test_images.py

Generate synthetic test images for testing thresholding algorithms.
Creates various test cases with known properties for validation.

Test Cases:
1. Simple bimodal - Clear foreground/background separation
2. Gradient - Smooth intensity gradient  
3. Checkerboard - Regular pattern
4. Noisy bimodal - Bimodal with Gaussian noise
5. Circle - Non-rectangular foreground region
"""

import numpy as np
import os

def save_image_txt(arr, filename):
    """Save numpy array as text format: H W C followed by pixel data."""
    H, W = arr.shape
    with open(filename, 'w') as f:
        f.write(f"{H} {W} 1\n")
        for y in range(H):
            row = ' '.join(str(int(v)) for v in arr[y])
            f.write(row + '\n')
    print(f"Created: {filename} ({W}x{H}, {W*H:,} pixels)")

def generate_bimodal_image(width=256, height=256, bg_val=50, fg_val=200):
    """
    Create image with clear bimodal distribution.
    Left half: background (dark), Right half: foreground (bright)
    Expected Otsu threshold: approximately (bg_val + fg_val) / 2 = ~125
    """
    img = np.zeros((height, width), dtype=np.uint8)
    img[:, :width//2] = bg_val
    img[:, width//2:] = fg_val
    return img

def generate_circle_image(width=256, height=256, bg_val=30, fg_val=220):
    """
    Create image with circular foreground.
    Tests threshold with non-rectangular regions.
    """
    img = np.full((height, width), bg_val, dtype=np.uint8)
    cy, cx = height // 2, width // 2
    radius = min(width, height) // 3
    
    for y in range(height):
        for x in range(width):
            if (x - cx)**2 + (y - cy)**2 <= radius**2:
                img[y, x] = fg_val
    return img

def generate_gradient_image(width=256, height=256):
    """
    Create horizontal intensity gradient (0 to 255).
    Tests threshold at various intensity levels.
    Expected Otsu threshold: ~127 (midpoint)
    """
    img = np.zeros((height, width), dtype=np.uint8)
    for x in range(width):
        img[:, x] = int(255 * x / (width - 1))
    return img

def generate_checkerboard_image(width=256, height=256, block_size=32, 
                                 dark=0, bright=255):
    """
    Create checkerboard pattern.
    Expected Otsu threshold: ~127 (equal areas of dark/bright)
    """
    img = np.zeros((height, width), dtype=np.uint8)
    for y in range(height):
        for x in range(width):
            block_y = y // block_size
            block_x = x // block_size
            if (block_x + block_y) % 2 == 0:
                img[y, x] = dark
            else:
                img[y, x] = bright
    return img

def generate_noisy_bimodal_image(width=256, height=256, 
                                  bg_mean=60, fg_mean=200, noise_std=20):
    """
    Create bimodal image with Gaussian noise.
    Tests robustness of Otsu's method.
    """
    np.random.seed(42)  # Reproducible
    img = np.zeros((height, width), dtype=np.float32)
    
    # Background (left half) with noise
    img[:, :width//2] = bg_mean + np.random.normal(0, noise_std, (height, width//2))
    
    # Foreground (right half) with noise  
    img[:, width//2:] = fg_mean + np.random.normal(0, noise_std, (height, width - width//2))
    
    img = np.clip(img, 0, 255).astype(np.uint8)
    return img

def generate_three_level_image(width=256, height=256):
    """
    Create image with three distinct intensity levels.
    Tests basic Otsu (which finds one threshold).
    Levels: 30, 128, 230
    """
    img = np.zeros((height, width), dtype=np.uint8)
    third = width // 3
    
    img[:, :third] = 30           # Dark region
    img[:, third:2*third] = 128   # Medium region
    img[:, 2*third:] = 230        # Bright region
    
    return img

def generate_large_test_image(width=1024, height=1024):
    """
    Generate larger image for performance testing.
    Mixed content: gradient, solid regions, noise.
    """
    np.random.seed(123)
    img = np.zeros((height, width), dtype=np.uint8)
    
    # Quadrant 1 (top-left): Gradient
    for x in range(width//2):
        img[:height//2, x] = int(255 * x / (width//2 - 1))
    
    # Quadrant 2 (top-right): Solid bright
    img[:height//2, width//2:] = 200
    
    # Quadrant 3 (bottom-left): Solid dark  
    img[height//2:, :width//2] = 50
    
    # Quadrant 4 (bottom-right): Random noise
    img[height//2:, width//2:] = np.random.randint(0, 256, 
                                                    (height - height//2, width - width//2),
                                                    dtype=np.uint8)
    return img

def generate_very_large_image(width=4096, height=4096):
    """
    Generate very large image for scalability/speedup testing.
    ~16 million pixels - good for demonstrating parallel speedup.
    """
    print(f"  Generating {width}x{height} image ({width*height:,} pixels)...")
    
    np.random.seed(42)
    
    # Bimodal with noise for realistic testing
    img = np.zeros((height, width), dtype=np.float32)
    
    # Top half: dark background with noise
    img[:height//2, :] = np.random.normal(60, 30, (height//2, width))
    
    # Bottom half: bright foreground with noise
    img[height//2:, :] = np.random.normal(180, 30, (height - height//2, width))
    
    img = np.clip(img, 0, 255).astype(np.uint8)
    return img


def main():
    """Generate all test images."""
    
    test_dir = "test_images"
    os.makedirs(test_dir, exist_ok=True)
    
    print("=" * 60)
    print("Generating Test Images for Thresholding Algorithms")
    print("=" * 60)
    
    # Basic test cases (256x256 = 65,536 pixels)
    test_cases = [
        ("test_bimodal.txt", generate_bimodal_image(),
         "Bimodal: left=50, right=200. Expected Otsu ~125"),
        
        ("test_circle.txt", generate_circle_image(),
         "Circle: bg=30, fg=220. Non-rectangular foreground"),
        
        ("test_gradient.txt", generate_gradient_image(),
         "Gradient: 0->255 horizontal. Otsu ~127"),
        
        ("test_checkerboard.txt", generate_checkerboard_image(),
         "Checkerboard: 0/255 blocks. Otsu ~127"),
        
        ("test_noisy.txt", generate_noisy_bimodal_image(),
         "Noisy bimodal: tests Otsu robustness"),
        
        ("test_three_level.txt", generate_three_level_image(),
         "Three levels: 30/128/230"),
    ]
    
    print("\n--- Standard Test Images (256x256) ---")
    for filename, img, description in test_cases:
        path = os.path.join(test_dir, filename)
        save_image_txt(img, path)
        print(f"    {description}")
    
    # Performance test images
    print("\n--- Performance Test Images ---")
    
    # Medium scale
    large_img = generate_large_test_image(1024, 1024)
    save_image_txt(large_img, os.path.join(test_dir, "test_1024x1024.txt"))
    print(f"    1024x1024 - Medium scale parallel testing")
    
    # Large scale  
    xlarge_img = generate_large_test_image(2048, 2048)
    save_image_txt(xlarge_img, os.path.join(test_dir, "test_2048x2048.txt"))
    print(f"    2048x2048 - Large scale parallel testing")
    
    # Very large for speedup demonstration
    print("\n--- Stress Test Image ---")
    vlarge_img = generate_very_large_image(4096, 4096)
    save_image_txt(vlarge_img, os.path.join(test_dir, "test_4096x4096.txt"))
    print(f"    4096x4096 - Speedup/scaling tests")
    
    print("\n" + "=" * 60)
    print("Test Image Generation Complete!")
    print("=" * 60)
    print(f"\nImages saved to: {os.path.abspath(test_dir)}/")
    
    print("\n--- Quick Test Commands ---")
    print("\nSequential Otsu (mlton):")
    print("  ./threshold test_images/test_bimodal.txt output.txt otsu")
    print("\nParallel Otsu (mpl):")
    print("  ./threshold_par test_images/test_bimodal.txt output.txt otsu")
    print("\nNormal thresholding with threshold=128:")
    print("  ./threshold test_images/test_gradient.txt output.txt normal 128")


if __name__ == "__main__":
    main()

