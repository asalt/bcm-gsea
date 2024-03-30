-- SQL commands to initialize database schema
CREATE TABLE IF NOT EXISTS comparison_groups (
    group_id INTEGER PRIMARY KEY,
    group_name TEXT
);
CREATE TABLE IF NOT EXISTS pathways (
    pathway_id INTEGER PRIMARY KEY,
    pathway_name TEXT,
    description TEXT
);
CREATE TABLE IF NOT EXISTS gsea_parameters (
    param_id INTEGER PRIMARY KEY,
    min_size INTEGER,
    max_size INTEGER,
    nperm INTEGER
);
CREATE TABLE IF NOT EXISTS fgsea_results (
    result_id INTEGER PRIMARY KEY,
    group_id INTEGER,
    pathway_id INTEGER,
    param_id INTEGER,
    pval REAL,
    padj REAL,
    nes REAL,
    stored_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (group_id) REFERENCES comparison_groups(group_id),
    FOREIGN KEY (pathway_id) REFERENCES pathways(pathway_id),
    FOREIGN KEY (param_id) REFERENCES gsea_parameters(param_id)
);
