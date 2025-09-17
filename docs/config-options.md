# BCM-GSEA Configuration Reference

This document lists all supported keys in the bcm-gsea configuration TOML files, including default values and notes on behaviour. You can also explore the schema from the CLI, e.g.:

```
$ bcm-gsea describe
$ bcm-gsea describe params.bubbleplot --json
```

## Top-level Structure

```
[params]
[params.barplot]
[params.barplot.advanced]
[params.bubbleplot]
[params.bubbleplot.advanced]
[params.enplot]
[params.heatmap_gsea]
[params.heatmap_gene]
[params.pca]
[params.extra]
[params.genesets]
[params.advanced]
```

## params

- `ranks_from` (string, default `""`): Data source, `"volcano"` or `"gct"`.
- `savedir` (string, default `./plots`): Root output directory (resolved relative to run cwd).
- `volcanodir` (string): Folder containing volcano TSV files (required when `ranks_from = "volcano"`).
- `rankfiledir` (string, default `"savedir"`): Where generated rank files are written; special value `"savedir"` puts them under `savedir/ranks`.
- `gct_path` (string): Path to GCT file (required when `ranks_from = "gct"`).
- `species` (string, default `"Homo sapiens"`): Species name passed to msigdbr.
- `zscore_emat` (bool, default `true`): Z-score expression matrix before analysis.
- `zscore_emat_groupby` (string/bool, default `false`): Grouping column for Z-score normalisation.
- `cut_by` (string, default `"group"`): Metadata column controlling chart faceting.

## params.barplot

- `limit` (array of ints, default `[12,20,32]`): `top N` sizes to plot individually/combined.
- `do_individual` (bool, default `true`): Render barplots per comparison.
- `do_combined` (bool, default `true`): Render aggregated barplots.

### params.barplot.advanced

- `stroke_width` (float, default `1.0`): Outline width for bar panels.

## params.bubbleplot

- `limit` (array of ints, default `[12,20,32]`): `top N` sizes for bubble plots.
- `do_individual` (bool, default `true`): Render per-comparison bubbles.
- `do_combined` (bool, default `true`): Render combined bubbles per gene set.
- `glyph` (string, default `"⁕"`): Symbol for `padj < 0.05` markers.

### params.bubbleplot.advanced

- `stroke_alpha` (float, default `0.55`): Outer ring transparency (`0`–`1`).
- `stroke_width` (float, default `0.8`): Outline width for bubble points.

## params.enplot

- `do_individual` (bool, default `true`)
- `do_combined` (bool, default `true`)
- `limit` (int, default `10`)
- `combine_by` (string/bool, default `false`)

## params.heatmap_gsea

- `do` (bool, default `true`)
- `limit` (array of ints, default `[10,20,40,80]`)
- `cut_by` (string): Override for grouping.
- `cluster_rows` (bool, default `true`)
- `cluster_columns` (array of bool, default `[false,true]`)
- `legend_include` (array of strings): Metadata columns to add to legend.

## params.heatmap_gene

- `do` (bool, default `true`)
- `limit` (int, default `10`)

## params.pca

- `do` (bool, default `false`)
- `width` (float, default `7.8`)
- `height` (float, default `7.4`)
- `col_by` (string)
- `mark_by` (string)
- `top_pc` (int)
- `max_pc` (int)
- `labSize`, `pointSize`, `sizeLoadingsNames` (floats)

## params.extra

- `rankname_order` (array of strings)
- `samplename_order` (array of strings)
- `sample_order` (array of strings)

## params.genesets (array of tables)

Each entry defines a gene-set collection:

- `category` (string)
- `subcategory` (string)
- `collapse` (bool)

## params.advanced

- `parallel` (bool, default `true`)
- `cache` (bool, default `true`)
- `replace` (bool, default `false`)
- `cachedir` (string, default `"savedir"` → `savedir/cache`)
- `logfile` (string, default `${savedir}/run.log`)
- `loglevel` (string, default `"INFO"`)
- `pivot_gsea_results` (bool, default `false`)
- `quiet` (bool, default `false`)
