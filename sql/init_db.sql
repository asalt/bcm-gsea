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

-- Create RankObjects table
CREATE TABLE IF NOT EXISTS RankObjects (
    rankobj_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    species TEXT DEFAULT "Homo sapiens"
);

CREATE TABLE IF NOT EXISTS Ranks (
    rank_id INTEGER PRIMARY KEY AUTOINCREMENT,
    rankobj_id INTEGER,
    id_type TEXT,
    bio_idstr TEXT NOT NULL,
    rank INTEGER,
    value REAL,
    FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id),
    UNIQUE (rankobj_id, bio_idstr)
);

CREATE TABLE IF NOT EXISTS Collections(
    collection_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT
);


CREATE TABLE IF NOT EXISTS CollectionResults (
    result_id INTEGER PRIMARY KEY AUTOINCREMENT,
    rankobj_id INTEGER,
    collection_id INTEGER,
    pathway_id INTEGER,
    pathway TEXT,
--param_id INTEGER,
    pval REAL,
    padj REAL,
    log2err REAL,
    es REAL,
    nes REAL,
    size INTEGER,
    leadingEdge TEXT,
    mainpathway INTEGER,
    stored_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id),
    FOREIGN KEY (collection_id) REFERENCES Collections(collection_id),
    FOREIGN KEY (pathway_id) REFERENCES Pathways(pathway_id),
    UNIQUE (rankobj_id, collection_id, pathway_id)
--  FOREIGN KEY (param_id) REFERENCES gsea_parameters(param_id)
);


-- Create Pathways table
-- ids is wide form, array of characterS
CREATE TABLE IF NOT EXISTS Pathways (
    pathway_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    ids TEXT,
    id_type TEXT DEFAULT "entrez",
    species TEXT DEFAULT "Homo sapiens",
    collection_id INTEGER,
    FOREIGN KEY (collection_id) REFERENCES Collections(collection_id)
);

-- -- Create Stats table (full stat rank object)
-- CREATE TABLE IF NOT EXISTS Stats (
--     rankobj_id INTEGER,
--     rank INTEGER,
--     stat REAL,
--     pathway_id INTEGER,
--     PRIMARY KEY (rankobj_id, rank),
--     FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id)
--     FOREIGN KEY (pathway_id) REFERENCES Pathways(pathway_id)
-- );

-- -- Create Ticks table
-- CREATE TABLE IF NOT EXISTS Ticks (
--     rankobj_id INTEGER,
--     pathway_id INTEGER,
--     rank INTEGER,
--     stat REAL,
--     PRIMARY KEY (rankobj_id, pathway_id, rank),
--     FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id),
--     FOREIGN KEY (pathway_id) REFERENCES Pathways(pathway_id)
-- );

-- -- Create Edge table
-- CREATE TABLE IF NOT EXISTS Edge (
--     rankobj_id INTEGER,
--     pathway_id INTEGER,
--     id INTEGER,
--     rank INTEGER,
--     stat REAL,
--     ES REAL,
--     stat_tick REAL,
--     PRIMARY KEY (rankobj_id, pathway_id, id),
--     FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id),
--     FOREIGN KEY (pathway_id) REFERENCES Pathways(pathway_id)
-- );

-- Create Curve table
CREATE TABLE IF NOT EXISTS Curves (
    curve_id INTEGER PRIMARY KEY AUTOINCREMENT,
    rankobj_id INTEGER,
    pathway_id INTEGER,
    rank REAL,
    ES REAL,
    FOREIGN KEY (rankobj_id) REFERENCES RankObjects(rankobj_id),
    FOREIGN KEY (pathway_id) REFERENCES Pathways(pathway_id)
);

-- Create indexes for better query performance
-- CREATE INDEX IF NOT EXISTS idx_stats_rank ON Stats(rank);
-- CREATE INDEX IF NOT EXISTS idx_ticks_rank ON Ticks(rank);
-- CREATE INDEX IF NOT EXISTS idx_edge_rank ON Edge(rank);
-- CREATE INDEX IF NOT EXISTS idx_curve_rank ON Curve(rank);
