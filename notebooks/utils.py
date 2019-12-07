import pandas as pd


def load_suppliers(path: str) -> pd.DataFrame:
    """Load and process suppliers data"""
    df = pd.read_json(path).drop(['_id', 'roles', 'identifier'], axis=1)
    df = df.rename(columns={'name': 'supplier_name', 'id': 'supplier_id'})
    df = df.join(pd.DataFrame([r for r in df.address]))
    df = df.join(pd.DataFrame([r for r in df.contactPoint]))
    df = df.rename(columns={'telephone': 'contact_telephone', 'name': 'contact_name'})
    df = df.drop(['address', 'contactPoint'], axis=1)
    return df
