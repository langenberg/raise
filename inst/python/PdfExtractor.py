import pymupdf4llm
import unicodedata

class PdfExtractor:
    def __init__(self):
        pass
    
    def extract(self, paths):
        result = []
        for path in paths:
            md_text = pymupdf4llm.to_markdown(path)
            md_text_normalized = unicodedata.normalize('NFKC', md_text)
                
            result.append({"path": path, "content": md_text, "content_normalized": md_text_normalized})
        return result
