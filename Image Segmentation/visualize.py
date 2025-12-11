import numpy as np
import matplotlib.pyplot as plt
from PIL import Image
import os
import sys
import glob

def load_segmented_image(txt_path):
    """
    Load image from text file format:
      First line: H W C
      Remaining: pixel values
        - C=1: H lines, each W ints
        - C=3: H lines, each 3*W ints (R G B ...)
    Returns (img_array, C)
    """
    with open(txt_path, "r") as f:
        header = f.readline().strip()
        if not header:
            raise ValueError("Empty file or missing header")
        parts = header.split()
        if len(parts) != 3:
            raise ValueError("Header must have 3 integers: H W C")
        H, W, C = map(int, parts)

    # Load remaining data as 2D array
    data = np.loadtxt(txt_path, dtype=np.uint8, skiprows=1)

    # Flatten then reshape according to header
    flat = data.reshape(-1)

    if C == 1:
        img = flat.reshape(H, W)
    elif C == 3:
        img = flat.reshape(H, W, C)
    else:
        raise ValueError(f"Unsupported number of channels: {C}")

    return img, C

def show_and_save_image(img_array, channels, output_png_path="segmented_image.png", 
                        title="Segmented Image", show=True):
    """
    Show the image using matplotlib and also save it as a PNG.
    """
    if channels == 1:
        plt.imshow(img_array, cmap="gray", vmin=0, vmax=255)
        mode = "L"
    else:
        plt.imshow(img_array)
        mode = "RGB"

    plt.axis("off")
    plt.title(title)
    
    if show:
        plt.show()

    img = Image.fromarray(img_array, mode=mode)
    img.save(output_png_path)
    print(f"Saved: {output_png_path}")

def visualize_comparison(input_path, output_path, save_path=None):
    """
    Show original and segmented images side by side.
    """
    fig, axes = plt.subplots(1, 2, figsize=(12, 5))
    
    # Load and show original
    if os.path.exists(input_path):
        img_in, c_in = load_segmented_image(input_path)
        if c_in == 1:
            axes[0].imshow(img_in, cmap="gray", vmin=0, vmax=255)
        else:
            axes[0].imshow(img_in)
        axes[0].set_title("Original")
        axes[0].axis("off")
    
    # Load and show output
    if os.path.exists(output_path):
        img_out, c_out = load_segmented_image(output_path)
        if c_out == 1:
            axes[1].imshow(img_out, cmap="gray", vmin=0, vmax=255)
        else:
            axes[1].imshow(img_out)
        axes[1].set_title("Segmented")
        axes[1].axis("off")
    
    plt.tight_layout()
    
    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"Saved comparison: {save_path}")
    
    plt.show()

def visualize_all_outputs(output_dir="output", save_dir="visualizations"):
    """
    Visualize all output files in a directory.
    """
    if not os.path.exists(output_dir):
        print(f"Output directory not found: {output_dir}")
        return
    
    os.makedirs(save_dir, exist_ok=True)
    
    txt_files = glob.glob(os.path.join(output_dir, "*.txt"))
    
    if not txt_files:
        print(f"No .txt files found in {output_dir}")
        return
    
    print(f"Found {len(txt_files)} output files")
    
    # Create grid visualization
    n = len(txt_files)
    cols = min(4, n)
    rows = (n + cols - 1) // cols
    
    fig, axes = plt.subplots(rows, cols, figsize=(4*cols, 4*rows))
    if n == 1:
        axes = np.array([axes])
    axes = axes.flatten()
    
    for i, txt_file in enumerate(sorted(txt_files)):
        try:
            img, c = load_segmented_image(txt_file)
            basename = os.path.basename(txt_file).replace('.txt', '')
            
            if c == 1:
                axes[i].imshow(img, cmap="gray", vmin=0, vmax=255)
            else:
                axes[i].imshow(img)
            axes[i].set_title(basename, fontsize=8)
            axes[i].axis("off")
            
            # Save individual PNG
            png_path = os.path.join(save_dir, f"{basename}.png")
            Image.fromarray(img, mode="L" if c == 1 else "RGB").save(png_path)
            
        except Exception as e:
            print(f"Error loading {txt_file}: {e}")
            axes[i].text(0.5, 0.5, "Error", ha='center', va='center')
            axes[i].axis("off")
    
    # Hide unused subplots
    for i in range(n, len(axes)):
        axes[i].axis("off")
    
    plt.tight_layout()
    overview_path = os.path.join(save_dir, "all_outputs.png")
    plt.savefig(overview_path, dpi=150, bbox_inches='tight')
    print(f"\nSaved overview: {overview_path}")
    plt.show()

def main():
    """Main entry point with command line argument handling."""
    
    if len(sys.argv) < 2:
        # Default behavior: visualize all outputs
        print("Usage:")
        print("  python visualize.py <file.txt>              - Visualize single file")
        print("  python visualize.py <input.txt> <output.txt> - Compare input/output")
        print("  python visualize.py --all                   - Visualize all in output/")
        print("")
        
        # Try default file locations
        default_files = [
            "threshold_output.txt",
            "threshold_par_output.txt", 
            "segmented_image.txt",
            "canny_edges.txt"
        ]
        
        for f in default_files:
            if os.path.exists(f):
                print(f"Found {f}, visualizing...")
                img_array, C = load_segmented_image(f)
                print(f"Loaded: shape={img_array.shape}, channels={C}")
                show_and_save_image(img_array, C, f.replace('.txt', '.png'))
                return
        
        # If output directory exists, visualize all
        if os.path.exists("output"):
            print("Visualizing all outputs...")
            visualize_all_outputs()
        else:
            print("No output files found. Run segmentation first.")
        return
    
    arg = sys.argv[1]
    
    if arg == "--all":
        visualize_all_outputs()
    elif len(sys.argv) == 2:
        # Single file
        img_array, C = load_segmented_image(arg)
        print(f"Loaded: shape={img_array.shape}, channels={C}")
        output_png = arg.replace('.txt', '.png')
        show_and_save_image(img_array, C, output_png)
    elif len(sys.argv) >= 3:
        # Compare input and output
        visualize_comparison(sys.argv[1], sys.argv[2])


if __name__ == "__main__":
    main()

