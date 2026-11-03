"""Agreement between the colour functions of the Python back end and of the R package.

The reference values in r_colour_pairs.csv and r_rgb_lab.csv were computed with fr_delta_e(), fr_colour_name() and
fr_rgb_to_lab(). Run from the package root: python -m pytest python-tests
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "inst" / "python"))

from fruittrait.colour import ciede2000, colour_name, linear_to_srgb, srgb_to_linear  # noqa: E402


@pytest.fixture(scope="module")
def pairs():
    return pd.read_csv(HERE / "r_colour_pairs.csv")


def test_ciede2000_reproduces_a_published_pair():
    # Sharma, Wu and Dalal (2005), pair 1
    assert ciede2000([50, 2.6772, -79.7751], [50, 0, -82.7485]) == pytest.approx(2.0425, abs=1e-4)
    assert ciede2000([40, 10, 10], [40, 10, 10]) == pytest.approx(0, abs=1e-12)


def test_ciede2000_matches_r(pairs):
    py = ciede2000(pairs[["L1", "a1", "b1"]].to_numpy(), pairs[["L2", "a2", "b2"]].to_numpy())
    np.testing.assert_allclose(py, pairs["dE_r"].to_numpy(), atol=1e-8)


def test_colour_names_match_r(pairs):
    py = colour_name(pairs["L1"].to_numpy(), pairs["a1"].to_numpy(), pairs["b1"].to_numpy())
    assert list(py) == list(pairs["name_r"])


def test_srgb_round_trip():
    x = np.linspace(0, 1, 1001)
    np.testing.assert_allclose(linear_to_srgb(srgb_to_linear(x)), x, atol=1e-12)


def test_rgb_to_lab_matches_r():
    cv2 = pytest.importorskip("cv2")
    ref = pd.read_csv(HERE / "r_rgb_lab.csv")
    img = ref[["red", "green", "blue"]].to_numpy(np.float32).reshape(-1, 1, 3) / 255
    lin = np.where(img <= 0.04045, img / 12.92, ((img + 0.055) / 1.055) ** 2.4).astype(np.float32)
    lab = cv2.cvtColor(lin, cv2.COLOR_LRGB2Lab).reshape(-1, 3)
    np.testing.assert_allclose(lab, ref[["L", "a", "b"]].to_numpy(), atol=0.02)


def test_fruit_colours_leave_out_the_brightest_pixels():
    pytest.importorskip("torch")
    from fruittrait.photo import fruit_colours, lab_image

    img = np.zeros((20, 20, 3), np.uint8)
    img[...] = (150, 20, 35)
    img[0, 0] = (255, 255, 255)
    masks = [np.zeros((20, 20), bool), np.zeros((20, 20), bool), np.zeros((20, 20), bool)]
    masks[0][:10] = True
    masks[1][10:, :10] = True
    masks[2][15, 15] = True
    V, names = fruit_colours(lab_image(img), masks)
    ref = lab_image(np.array([[[150, 20, 35]]], np.uint8))[0, 0]
    np.testing.assert_allclose(V[0], ref, atol=1e-4)
    np.testing.assert_allclose(V[1], ref, atol=1e-4)
    assert np.isnan(V[2]).all()
    assert names == ["red", "red", ""]
