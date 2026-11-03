"""fruittrait: colour of single fleshy fruits from field photographs (back end of the FruitTrait R package).

    photo.py        photo -> Grounding DINO-base (two stages) -> crop -> SAM 2.1-B outlines of single fruits ->
                    per-fruit outline checks -> fruit colour, colour of the surroundings, gloss, fruit number
    photofilter.py  fruit features (OpenCLIP embeddings of each fruit in context and alone, outline and colour
                    metrics) and fruit heads: single fruit with a correct outline, ripe, dry
    pipeline.py     full analysis of one photo: photo record, fruit rows, coordinates in the original photo
    colour.py       colour spaces, colour classes and colour names
    run_photos.py   batch CLI: CSV of photos (paths or URLs) -> traits.csv, fruits.csv, crops, label maps, overlays
The heavy imports (torch, transformers, ultralytics, open_clip) are kept inside photo.py and photofilter.py.
"""
__version__ = "0.1.0"
