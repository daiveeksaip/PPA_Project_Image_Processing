from PIL import Image
import sys
import os

def convert_to_txt(img_path, txt_path):
    try:
        # Open image and convert to grayscale
        img = Image.open(img_path).convert('L')
        width, height = img.size
        pixels = list(img.getdata())

        print(f"Converting {img_path} ({width}x{height}) to {txt_path}...")

        with open(txt_path, 'w') as f:
            # Write header: Height Width Channels
            f.write(f"{height} {width} 1\n")
            
            # Write pixels (space separated)
            # Writing one row at a time for readability/performance
            for i in range(0, len(pixels), width):
                row = pixels[i:i+width]
                f.write(" ".join(map(str, row)) + "\n")

        print("Done!")
    except Exception as e:
        print(f"Error converting to text: {e}")

def convert_to_img(txt_path, img_path):
    try:
        print(f"Converting {txt_path} to {img_path}...")
        
        with open(txt_path, 'r') as f:
            # Read header
            header = f.readline().strip().split()
            if not header:
                print("Error: Empty file")
                return
            
            if len(header) != 3:
                print(f"Error: Invalid header format: {header}")
                return
                
            height, width, channels = map(int, header)
            
            # Read pixels
            pixels = []
            for line in f:
                # Filter out empty strings from splitting multiple spaces
                vals = [int(x) for x in line.strip().split()]
                pixels.extend(vals)
        
        if len(pixels) != width * height:
            print(f"Warning: Expected {width*height} pixels, got {len(pixels)}")
            
        # Create image
        img = Image.new('L', (width, height))
        img.putdata(pixels)
        img.save(img_path)
        print("Done!")
    except Exception as e:
        print(f"Error converting to image: {e}")

if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("Usage:")
        print("  To text:  python3 convert_image.py to_txt input.png output.txt")
        print("  To image: python3 convert_image.py to_img input.txt output.png")
        sys.exit(1)
        
    mode = sys.argv[1]
    input_file = sys.argv[2]
    output_file = sys.argv[3]
    
    if mode == "to_txt":
        convert_to_txt(input_file, output_file)
    elif mode == "to_img":
        convert_to_img(input_file, output_file)
    else:
        print("Unknown mode. Use 'to_txt' or 'to_img'")

