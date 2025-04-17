import pandas as pd
import os

def detect_delimiter(filepath, n_lines=10):
    delimiters = [',', '\t', ';', '|', ' ']
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = [next(f) for _ in range(n_lines)]
    text_sample = ''.join(lines)
    counts = {d: text_sample.count(d) for d in delimiters}
    delimiter = max(counts, key=counts.get)
    # Prefer tab if both tab and space are present
    if delimiter == ' ' and '\t' in text_sample:
        delimiter = '\t'
    return delimiter

def is_likely_rownames(column):
    # Unique and mostly non-numeric
    if len(set(column)) != len(column):
        return False
    try:
        numeric_count = sum(pd.to_numeric(column, errors='coerce').notna())
    except Exception:
        numeric_count = 0
    return numeric_count / len(column) < 0.9

def is_likely_header(row):
    numeric_count = sum(pd.to_numeric(row, errors='coerce').notna())
    return numeric_count / len(row) < 0.5

def load_data(filepath, sheet_name=None, delimiter=None, header='infer', rownames=False):
    ext = os.path.splitext(filepath)[1].lower()
    if ext in ['.xls', '.xlsx']:
        df = pd.read_excel(filepath, sheet_name=sheet_name, header=0 if header else None)
    else:
        if delimiter is None:
            delimiter = detect_delimiter(filepath)
        df = pd.read_csv(filepath, delimiter=delimiter, header=0 if header else None)
    if rownames and df.shape[1] > 1:
        df.index = df.iloc[:, 0]
        df = df.iloc[:, 1:]
    return df 