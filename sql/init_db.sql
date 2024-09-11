-- SQL commands to initialize database schema
-- CREATE TABLE IF NOT EXISTS rank (
--     group_id INTEGER PRIMARY KEY,
--     group_name TEXT
-- );
-- CREATE TABLE IF NOT EXISTS pathways (
--     pathway_id INTEGER PRIMARY KEY,
--     pathway_name TEXT,
--     description TEXT
-- );
-- CREATE TABLE IF NOT EXISTS gsea_parameters (
--     param_id INTEGER PRIMARY KEY,
--     min_size INTEGER,
--     max_size INTEGER,
--     nperm INTEGER
-- );
-- CREATE TABLE IF NOT EXISTS fgsea_results (
--     result_id INTEGER PRIMARY KEY,
--     group_id INTEGER,
--     pathway_id INTEGER,
--     param_id INTEGER,
--     pval REAL,
--     padj REAL,
--     nes REAL,
--     stored_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
--     FOREIGN KEY (group_id) REFERENCES comparison_groups(group_id),
--     FOREIGN KEY (pathway_id) REFERENCES pathways(pathway_id),
--     FOREIGN KEY (param_id) REFERENCES gsea_parameters(param_id)
-- );


-- Create RankObjects table
CREATE TABLE IF NOT EXISTS RankObjects (
    rankobj_id INTEGER PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS Collection(
    collection_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT
);

-- Create Pathways table
-- ids is wide form, array of characterS
CREATE TABLE IF NOT EXISTS Pathways (
    pathway_id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    ids TEXT,
    collection_id INTEGER,
    FOREIGN KEY (collection_id) REFERENCES Collection(collection_id)
);

-- Create Stats table (full stat rank object)
CREATE TABLE IF NOT EXISTS Stats (
    rankobj_id INTEGER,
    rank INTEGER,
    stat REAL,
    pathway_id INTEGER,
    PRIMARY KEY (rankobj_id, rank),
    FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id)
    FOREIGN KEY (pathway_id) REFERENCES Pathways(pathway_id)
);

-- Create Ticks table
CREATE TABLE IF NOT EXISTS Ticks (
    rankobj_id INTEGER,
    pathway_id INTEGER,
    rank INTEGER,
    stat REAL,
    PRIMARY KEY (rankobj_id, pathway_id, rank),
    FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id),
    FOREIGN KEY (pathway_id) REFERENCES Pathways(pathway_id)
);

-- Create Edge table
CREATE TABLE IF NOT EXISTS Edge (
    rankobj_id INTEGER,
    pathway_id INTEGER,
    id INTEGER,
    rank INTEGER,
    stat REAL,
    ES REAL,
    stat_tick REAL,
    PRIMARY KEY (rankobj_id, pathway_id, id),
    FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id),
    FOREIGN KEY (pathway_id) REFERENCES Pathways(pathway_id)
);

-- Create Curve table
CREATE TABLE IF NOT EXISTS Curve (
    rankobj_id INTEGER,
    pathway_id INTEGER,
    rank REAL,
    ES REAL,
    PRIMARY KEY (rankobj_id, pathway_id, rank),
    FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id),
    FOREIGN KEY (pathway_id) REFERENCES Pathways(pathway_id)
);

-- Create indexes for better query performance
CREATE INDEX IF NOT EXISTS idx_stats_rank ON Stats(rank);
CREATE INDEX IF NOT EXISTS idx_ticks_rank ON Ticks(rank);
CREATE INDEX IF NOT EXISTS idx_edge_rank ON Edge(rank);
CREATE INDEX IF NOT EXISTS idx_curve_rank ON Curve(rank);
