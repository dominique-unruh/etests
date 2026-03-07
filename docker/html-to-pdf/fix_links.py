import pikepdf, sys

src, dst, base = sys.argv[1], sys.argv[2], sys.argv[3]
# base e.g. "file:///workdir/"

with pikepdf.open(src) as pdf:
    for page in pdf.pages:
        for annot in page.get("/Annots", []):
            if "/A" in annot and "/URI" in annot["/A"]:
                uri = str(annot["/A"]["/URI"])
                if uri.startswith(base):
                    annot["/A"]["/URI"] = pikepdf.String(uri[len(base):])
    pdf.save(dst)