import numpy as np
from PIL import Image

def save_image_matrix(input_path, output_txt_path, grayscale=True):
    """
    Reads an image (JPG/PNG), optionally converts to grayscale,
    and writes a text file with header:
        H W C
    followed by rows of pixel data.

    Grayscale: each row has W integers.
    RGB:      each row has 3*W integers (R G B R G B ...).
    """
    img = Image.open(input_path)

    if grayscale:
        img = img.convert("L")      # 1 channel
        arr = np.array(img)         # (H, W)
        H, W = arr.shape
        C = 1
    else:
        img = img.convert("RGB")    # 3 channels
        arr = np.array(img)         # (H, W, 3)
        H, W, C = arr.shape

    with open(output_txt_path, "w") as f:
        # Header line
        f.write(f"{H} {W} {C}\n")

        if C == 1:
            # Each row: W grayscale values
            for y in range(H):
                row = " ".join(str(int(v)) for v in arr[y])
                f.write(row + "\n")
        else:
            # Each row: R G B R G B ... (3*W ints)
            for y in range(H):
                row_vals = []
                for x in range(W):
                    r, g, b = arr[y, x]
                    row_vals.extend([int(r), int(g), int(b)])
                f.write(" ".join(map(str, row_vals)) + "\n")

    print(f"Saved image matrix with header to {output_txt_path}")


if __name__ == "__main__":
    save_image_matrix(
        input_path="fruits.jpeg",
        output_txt_path="image_matrix.txt",
        grayscale=True  # set True for grayscale, False for RGB
    )

