/* -- access tacobell database
srun --time=23:00:00 --pty bash
module load mariadb/5.5.64
mysql -p -h db -P 33061 tacobell

-- run SQL squirrel on HPC
-- launch Xming first
srun --partition=cpu_short --ntasks=2 --cpus-per-task=1 --mem-per-cpu=8G --time=23:00:00 --x11 --pty bash
module load mariadb/5.5.64 squirrel/4.0.0
java -jar $SQUIRREL_ROOT/squirrel-sql.jar
*/

/*
+--------------+--------+-----------+------------+-------------+------------+--------------+---------------+-------------+--------+-------------------------+-------------------------+
| DW_GC_HEADER | DW_DAY | DW_RESTID | DW_PRODUCT | DW_LINEITEM | ACTQTYSOLD | ACTPRODPRICE | ACTGROSSSALES | ACTNETSALES | ACTTAX | LINEITEMDESC            | PRODUCTDESC             |
+--------------+--------+-----------+------------+-------------+------------+--------------+---------------+-------------+--------+-------------------------+-------------------------+
|   2193837648 |   6210 |     31704 |         -1 |          14 |       0.00 |         0.00 |          0.00 |        0.00 |   0.86 | TAX-LINE                | N/A                     |
|   2193837648 |   6210 |     31704 |        884 |           2 |       1.00 |         2.99 |          2.99 |        2.99 |   0.00 | NON-COMBO-ITEM          | GRILL STUFT BURRITO STK |
|   2193837648 |   6210 |     31704 |       1691 |           4 |       1.00 |         0.00 |          0.00 |        0.00 |   0.00 | COMBO-DETAIL-REPLS-PLUS | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       1691 |           3 |       1.00 |         0.00 |          0.00 |        0.00 |   0.00 | COMBO-DETAIL            | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       1691 |           4 |       1.00 |         0.00 |          0.00 |        0.00 |   0.00 | COMBO-DETAIL-REPLS-PLUS | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       1691 |           3 |       1.00 |         0.00 |          0.00 |        0.00 |   0.00 | COMBO-DETAIL            | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       1691 |           1 |       1.00 |         5.69 |          5.69 |        5.69 |   0.00 | COMBO-ITEM              | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       4035 |           2 |       1.00 |         1.69 |          1.69 |        1.69 |   0.00 | NON-COMBO-ITEM          | GRILLED STEAK TACO      |
|   2193837648 |   6210 |     31704 |         -1 |          14 |       0.00 |         0.00 |          0.00 |        0.00 |   0.86 | TAX-LINE                | N/A                     |
|   2193837648 |   6210 |     31704 |        884 |           2 |       1.00 |         2.99 |          2.99 |        2.99 |   0.00 | NON-COMBO-ITEM          | GRILL STUFT BURRITO STK |
|   2193837648 |   6210 |     31704 |       1691 |           4 |       1.00 |         0.00 |          0.00 |        0.00 |   0.00 | COMBO-DETAIL-REPLS-PLUS | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       1691 |           3 |       1.00 |         0.00 |          0.00 |        0.00 |   0.00 | COMBO-DETAIL            | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       1691 |           4 |       1.00 |         0.00 |          0.00 |        0.00 |   0.00 | COMBO-DETAIL-REPLS-PLUS | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       1691 |           3 |       1.00 |         0.00 |          0.00 |        0.00 |   0.00 | COMBO-DETAIL            | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       1691 |           1 |       1.00 |         5.69 |          5.69 |        5.69 |   0.00 | COMBO-ITEM              | COMBO 6                 |
|   2193837648 |   6210 |     31704 |       4035 |           2 |       1.00 |         1.69 |          1.69 |        1.69 |   0.00 | NON-COMBO-ITEM          | GRILLED STEAK TACO      |
+--------------+--------+-----------+------------+-------------+------------+--------------+---------------+-------------+--------+-------------------------+-------------------------+
*/

-- select sample transactions
select t.DW_GC_HEADER, t.DW_DAY, t.DW_RESTID, t.DW_PRODUCT, p.PRODUCTDESC, t.DW_LINEITEM, l.LINEITEMDESC, t.ACTQTYSOLD, t.ACTPRODPRICE, t.ACTGROSSSALES, t.ACTTAX, a.OPENEDDT, a.CLOSEDDT
from TLD_FACT t
    left join LINEITEM_DIM l using (DW_LINEITEM)
    left join PRODUCT_DIM p using (DW_PRODUCT)
    left join ALIGN_DIM a using (DW_RESTID)
where DW_GC_HEADER=2193837648
    or DW_GC_HEADER=2193837602
    or DW_GC_HEADER=2193837510
    or DW_GC_HEADER=2193838982
order by DW_GC_HEADER; --16 min 4.40 sec
/*

select t.DW_GC_HEADER, d.BUSIDAYDT, t.DW_PRODUCT, p.PRODUCTDESC, t.DW_PRODUCTDETAIL, detail.PRODUCTDESC, t.DW_PRODUCTMOD, m.PRODUCTDESC, t.DW_LINEITEM, l.LINEITEMDESC, t.ACTQTYSOLD, t.ACTPRODPRICE, t.ACTGROSSSALES, t.ACTTAX
from TLD_FACT_2007_Q01 t
	left join LINEITEM_DIM l using (DW_LINEITEM)
	left join PRODUCT_DIM p using (DW_PRODUCT)
	left join TIME_DAY_DIM d using (DW_DAY)
	left join PRODUCT_MODIFICATION_DIM_V1 m using (DW_PRODUCTMOD)
	left join PRODUCT_DETAIL_DIM_V1 detail using (DW_PRODUCTDETAIL)
where DW_GC_HEADER=2193837510
order by DW_LINEITEM; --34.27 sec
+--------------+------------+------------+-----------------------+------------------+-----------------------+---------------+-------------+-------------+-------------------------+------------+--------------+---------------+--------+
| DW_GC_HEADER | BUSIDAYDT  | DW_PRODUCT | PRODUCTDESC           | DW_PRODUCTDETAIL | PRODUCTDESC           | DW_PRODUCTMOD | PRODUCTDESC | DW_LINEITEM | LINEITEMDESC            | ACTQTYSOLD | ACTPRODPRICE | ACTGROSSSALES | ACTTAX |
+--------------+------------+------------+-----------------------+------------------+-----------------------+---------------+-------------+-------------+-------------------------+------------+--------------+---------------+--------+
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |            19494 | TB CRUNCHWRAP SUP CMB |            -1 | N/A         |           1 | COMBO-ITEM              |       1.00 |         3.69 |          3.69 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |            19494 | TB CRUNCHWRAP SUP CMB |            -1 | N/A         |           1 | COMBO-ITEM              |       1.00 |         3.69 |          3.69 |   0.00 |
|   2193837510 | 2006-12-27 |       4001 | SOFT TACO BEEF        |             4001 | SOFT TACO BEEF        |            -1 | N/A         |           2 | NON-COMBO-ITEM          |       2.00 |         0.79 |          1.58 |   0.00 |
|   2193837510 | 2006-12-27 |       4001 | SOFT TACO BEEF        |             4001 | SOFT TACO BEEF        |            -1 | N/A         |           2 | NON-COMBO-ITEM          |       2.00 |         0.79 |          1.58 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |             4450 | CRUNCHY TACO BEEF     |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |            19495 | TB CRUNCHWRAP SUPREME |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |             3649 | LARGE PEPSI           |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |            19495 | TB CRUNCHWRAP SUPREME |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |             4450 | CRUNCHY TACO BEEF     |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |             3649 | LARGE PEPSI           |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |            19495 | TB CRUNCHWRAP SUPREME |          2948 | LETTUCE     |           8 | COMBO-MOD-INGRD-MINUS   |       0.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |             4450 | CRUNCHY TACO BEEF     |          2948 | LETTUCE     |           8 | COMBO-MOD-INGRD-MINUS   |       0.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |             4450 | CRUNCHY TACO BEEF     |          2948 | LETTUCE     |           8 | COMBO-MOD-INGRD-MINUS   |       0.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |      19494 | TB CRUNCHWRAP SUP CMB |            19495 | TB CRUNCHWRAP SUPREME |          2948 | LETTUCE     |           8 | COMBO-MOD-INGRD-MINUS   |       0.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |       4001 | SOFT TACO BEEF        |             4001 | SOFT TACO BEEF        |          2948 | LETTUCE     |          11 | NON-COMBO-M-INGRD-MINUS |       0.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |       4001 | SOFT TACO BEEF        |             4001 | SOFT TACO BEEF        |          2948 | LETTUCE     |          11 | NON-COMBO-M-INGRD-MINUS |       0.00 |         0.00 |          0.00 |   0.00 |
|   2193837510 | 2006-12-27 |         -1 | N/A                   |               -1 | N/A                   |            -1 | N/A         |          14 | TAX-LINE                |       0.00 |         0.00 |          0.00 |   0.43 |
|   2193837510 | 2006-12-27 |         -1 | N/A                   |               -1 | N/A                   |            -1 | N/A         |          14 | TAX-LINE                |       0.00 |         0.00 |          0.00 |   0.43 |
+--------------+------------+------------+-----------------------+------------------+-----------------------+---------------+-------------+-------------+-------------------------+------------+--------------+---------------+--------+

select t.DW_GC_HEADER, d.BUSIDAYDT, t.DW_PRODUCT, p.PRODUCTDESC, t.DW_PRODUCTDETAIL, detail.PRODUCTDESC, t.DW_PRODUCTMOD, m.PRODUCTDESC, t.DW_LINEITEM, l.LINEITEMDESC, t.ACTQTYSOLD, t.ACTPRODPRICE, t.ACTGROSSSALES, t.ACTTAX
from TLD_FACT_2015_Q01 t
	left join LINEITEM_DIM l using (DW_LINEITEM)
	left join PRODUCT_DIM p using (DW_PRODUCT)
	left join TIME_DAY_DIM d using (DW_DAY)
	left join PRODUCT_MODIFICATION_DIM_V1 m using (DW_PRODUCTMOD)
	left join PRODUCT_DETAIL_DIM_V1 detail using (DW_PRODUCTDETAIL)
where DW_GC_HEADER=7008434308
order by DW_LINEITEM; --56.76 sec
+--------------+------------+------------+------------------+------------------+--------------------+---------------+-------------+-------------+-------------------------+------------+--------------+---------------+--------+
| DW_GC_HEADER | BUSIDAYDT  | DW_PRODUCT | PRODUCTDESC      | DW_PRODUCTDETAIL | PRODUCTDESC        | DW_PRODUCTMOD | PRODUCTDESC | DW_LINEITEM | LINEITEMDESC            | ACTQTYSOLD | ACTPRODPRICE | ACTGROSSSALES | ACTTAX |
+--------------+------------+------------+------------------+------------------+--------------------+---------------+-------------+-------------+-------------------------+------------+--------------+---------------+--------+
|   7008434308 | 2015-03-01 |      37591 | QUESARITO BOX    |            37591 | QUESARITO BOX      |            -1 | N/A         |           1 | COMBO-ITEM              |       1.00 |         7.18 |          7.18 |   0.00 |
|   7008434308 | 2015-03-01 |       1692 | COMBO 7          |             1692 | COMBO 7            |            -1 | N/A         |           1 | COMBO-ITEM              |       1.00 |         5.69 |          5.69 |   0.00 |
|   7008434308 | 2015-03-01 |      37202 | CRWRP SLDR CHD   |            37202 | CRWRP SLDR CHD     |            -1 | N/A         |           2 | NON-COMBO-ITEM          |       3.00 |         1.69 |          5.07 |   0.00 |
|   7008434308 | 2015-03-01 |      37667 | CW SLDR SPICY SC |            37667 | CW SLDR SPICY SC   |            -1 | N/A         |           2 | NON-COMBO-ITEM          |       3.00 |         1.69 |          5.07 |   0.00 |
|   7008434308 | 2015-03-01 |      37591 | QUESARITO BOX    |            34596 | DORITOS LOCOS TACO |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 | 2015-03-01 |       1692 | COMBO 7          |             3844 | QUESADILLA CHICKEN |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 | 2015-03-01 |      37591 | QUESARITO BOX    |            27270 | MEDIUM MT DEW      |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 | 2015-03-01 |       1692 | COMBO 7          |            27249 | LARGE PEPSI        |            -1 | N/A         |           3 | COMBO-DETAIL            |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 | 2015-03-01 |      37591 | QUESARITO BOX    |            38194 | SRI QUESRTO ST     |            -1 | N/A         |           4 | COMBO-DETAIL-REPLS-PLUS |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 | 2015-03-01 |       1692 | COMBO 7          |             4001 | SOFT TACO BEEF     |            -1 | N/A         |           6 | COMBO-DETAIL-REPLS-EQL  |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 | 2015-03-01 |      37591 | QUESARITO BOX    |             4001 | SOFT TACO BEEF     |            -1 | N/A         |           6 | COMBO-DETAIL-REPLS-EQL  |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 | 2015-03-01 |       1692 | COMBO 7          |             4001 | SOFT TACO BEEF     |          2948 | LETTUCE     |           8 | COMBO-MOD-INGRD-MINUS   |       0.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 | 2015-03-01 |         -1 | N/A              |               -1 | N/A                |            -1 | N/A         |          14 | TAX-LINE                |       0.00 |         0.00 |          0.00 |   1.61 |
+--------------+------------+------------+------------------+------------------+--------------------+---------------+-------------+-------------+-------------------------+------------+--------------+---------------+--------+

select t.DW_GC_HEADER, d.BUSIDAYDT, t.DW_PRODUCT, p.PRODUCTDESC, t.DW_PRODUCTDETAIL, detail.PRODUCTDESC, t.DW_PRODUCTMOD, m.PRODUCTDESC, t.DW_LINEITEM, l.LINEITEMDESC, t.ACTQTYSOLD, t.ACTPRODPRICE, t.ACTGROSSSALES, t.ACTTAX
from TLD_FACT_2007_Q02 t
	left join LINEITEM_DIM l using (DW_LINEITEM)
	left join PRODUCT_DIM p using (DW_PRODUCT)
	left join TIME_DAY_DIM d using (DW_DAY)
	left join PRODUCT_MODIFICATION_DIM_V1 m using (DW_PRODUCTMOD)
	left join PRODUCT_DETAIL_DIM_V1 detail using (DW_PRODUCTDETAIL)
where DW_GC_HEADER=2268477408
order by DW_LINEITEM; --19.49 sec
+--------------+------------+------------+----------------------------+------------------+----------------------------+---------------+-------------+-------------+----------------+------------+--------------+---------------+--------+
| DW_GC_HEADER | BUSIDAYDT  | DW_PRODUCT | PRODUCTDESC                | DW_PRODUCTDETAIL | PRODUCTDESC                | DW_PRODUCTMOD | PRODUCTDESC | DW_LINEITEM | LINEITEMDESC   | ACTQTYSOLD | ACTPRODPRICE | ACTGROSSSALES | ACTTAX |
+--------------+------------+------------+----------------------------+------------------+----------------------------+---------------+-------------+-------------+----------------+------------+--------------+---------------+--------+
|   2268477408 | 2007-03-21 |        457 | 7 LAYER BURRITO            |              457 | 7 LAYER BURRITO            |            -1 | N/A         |           2 | NON-COMBO-ITEM |       1.00 |         1.69 |          1.69 |   0.00 |
|   2268477408 | 2007-03-21 |       1163 | CHALUPA SUPREME STEAK      |             1163 | CHALUPA SUPREME STEAK      |            -1 | N/A         |           2 | NON-COMBO-ITEM |       2.00 |         2.09 |          4.18 |   0.00 |
|   2268477408 | 2007-03-21 |       2031 | DOUBLE DECKER TACO BEEF    |             2031 | DOUBLE DECKER TACO BEEF    |            -1 | N/A         |           2 | NON-COMBO-ITEM |       1.00 |         0.99 |          0.99 |   0.00 |
|   2268477408 | 2007-03-21 |       4450 | CRUNCHY TACO BEEF          |             4450 | CRUNCHY TACO BEEF          |            -1 | N/A         |           2 | NON-COMBO-ITEM |       2.00 |         0.79 |          1.58 |   0.00 |
|   2268477408 | 2007-03-21 |       5082 | RANCHERO CHICKEN SOFT TACO |             5082 | RANCHERO CHICKEN SOFT TACO |            -1 | N/A         |           2 | NON-COMBO-ITEM |       3.00 |         1.69 |          5.07 |   0.00 |
|   2268477408 | 2007-03-21 |         -1 | N/A                        |               -1 | N/A                        |            -1 | N/A         |          14 | TAX-LINE       |       0.00 |         0.00 |          0.00 |   1.11 |
+--------------+------------+------------+----------------------------+------------------+----------------------------+---------------+-------------+-------------+----------------+------------+--------------+---------------+--------+



-- connect product_dim and product_group_det
select p.DW_PRODUCT, p.PRODUCTDESC, p.DW_PRODUCTGROUP, d.PRODUCTGROUPDESC
from PRODUCT_DIM p
	left join PRODUCT_GROUP_DET d using(DW_PRODUCTGROUP)
where PRODUCTDESC="COMBO 4";
/*
+------------+-------------+-----------------+------------------+
| DW_PRODUCT | PRODUCTDESC | DW_PRODUCTGROUP | PRODUCTGROUPDESC |
+------------+-------------+-----------------+------------------+
|       1673 | COMBO 4     |              29 | COMBOS           |
|       2271 | COMBO 4     |              29 | COMBOS           |
|       1738 | COMBO 4     |              29 | COMBOS           |
|       1642 | COMBO 4     |              29 | COMBOS           |
|       1689 | COMBO 4     |              29 | COMBOS           |
|      36572 | COMBO 4     |              29 | COMBOS           |
|       3129 | COMBO 4     |              29 | COMBOS           |
|       4512 | COMBO 4     |              29 | COMBOS           |
+------------+-------------+-----------------+------------------+
*/

-- columns in question
-- DW_CHANNEL, DW_GC_HEADER
describe LINEITEM_SEQ_DIM;
/*
+----------------+-------------+------+-----+---------+-------+
| Field          | Type        | Null | Key | Default | Extra |
+----------------+-------------+------+-----+---------+-------+
| DW_LINEITEMSEQ | smallint(6) | NO   | PRI | NULL    |       |
| LINEITEMSEQ    | smallint(6) | NO   |     | NULL    |       |
+----------------+-------------+------+-----+---------+-------+
*/

select * from LINEITEM_SEQ_DIM; --nothing useful

-- use TLD_FACT_2015_Q01 as an example
select t.DW_GC_HEADER, t.DW_DAY, t.DW_RESTID, t.DW_PRODUCT, t.DW_LINEITEM, t.ACTQTYSOLD, t.ACTPRODPRICE, t.ACTGROSSSALES, t.ACTTAX, a.OPENEDDT, a.CLOSEDDT
from TLD_FACT_2015_Q01 t
    left join ALIGN_DIM a using (DW_RESTID)
where OPENEDDT!="0000-00-00"
	and CLOSEDDT="0000-00-00"
limit 20;
/*
+--------------+--------+-----------+------------+-------------+------------+--------------+---------------+--------+------------+------------+
| DW_GC_HEADER | DW_DAY | DW_RESTID | DW_PRODUCT | DW_LINEITEM | ACTQTYSOLD | ACTPRODPRICE | ACTGROSSSALES | ACTTAX | OPENEDDT   | CLOSEDDT   |
+--------------+--------+-----------+------------+-------------+------------+--------------+---------------+--------+------------+------------+
|   7008434308 |   9196 |    150215 |      37591 |           4 |       1.00 |         0.00 |          0.00 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434308 |   9196 |    150215 |       1692 |           3 |       1.00 |         0.00 |          0.00 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434308 |   9196 |    150215 |      37667 |           2 |       3.00 |         1.69 |          5.07 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434492 |   9196 |    150216 |        802 |           2 |       1.00 |         1.19 |          1.19 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434492 |   9196 |    150216 |      36021 |           1 |       1.00 |         4.29 |          4.29 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434492 |   9196 |    150216 |      36021 |           3 |       1.00 |         0.00 |          0.00 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434492 |   9196 |    150216 |      36935 |           2 |       1.00 |         3.39 |          3.39 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434216 |   9196 |    150215 |       1689 |           3 |       1.00 |         0.00 |          0.00 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434216 |   9196 |    150215 |      36021 |           3 |       1.00 |         0.00 |          0.00 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434262 |   9196 |    150215 |       3354 |          11 |       0.00 |         0.00 |          0.00 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434262 |   9196 |    150215 |       3354 |          11 |       0.00 |         0.00 |          0.00 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434308 |   9196 |    150215 |      37591 |           1 |       1.00 |         7.18 |          7.18 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434308 |   9196 |    150215 |       1692 |           1 |       1.00 |         5.69 |          5.69 |   0.00 | 2006-03-15 | 0000-00-00 |
|   7008434308 |   9196 |    150215 |      37202 |           2 |       3.00 |         1.69 |          5.07 |   0.00 | 2006-03-15 | 0000-00-00 |
+--------------+--------+-----------+------------+-------------+------------+--------------+---------------+--------+------------+------------+
*/

select * from TLD_FACT_2015_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492 order by DW_GC_HEADER; --runtime: 
/*
+--------------+--------+-----------+-----------+-------------+---------------+----------------+----------------------+------------+------------------+---------------+-------------+-------------+--------------+---------+------------+-------------+-----------+------------+--------------+---------------+-------------+---------------+--------------+--------+------------+---------------------------------+
| DW_GC_HEADER | DW_DAY | DW_MINUTE | DW_RESTID | DW_OCCASION | DW_TENDERTYPE | DW_LINEITEMSEQ | DW_LINEITEMSEQPARENT | DW_PRODUCT | DW_PRODUCTDETAIL | DW_PRODUCTMOD | DW_DISCOUNT | DW_LINEITEM | SEASONFACTOR | COUNTER | ACTQTYSOLD | ACTPROMOQTY | ACTMODQTY | ACTDISCQTY | ACTPRODPRICE | ACTGROSSSALES | ACTNETSALES | ACTPROMOSALES | ACTDISCSALES | ACTTAX | DW_CHANNEL | DW_CHECK_IN_LOCATION_IDENTIFIER |
+--------------+--------+-----------+-----------+-------------+---------------+----------------+----------------------+------------+------------------+---------------+-------------+-------------+--------------+---------+------------+-------------+-----------+------------+--------------+---------------+-------------+---------------+--------------+--------+------------+---------------------------------+
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |             10 |                    8 |      37591 |            38194 |            -1 |          -1 |           4 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |             12 |                    8 |      37591 |             4001 |            -1 |          -1 |           6 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |              5 |                    4 |       1692 |             4001 |          2948 |          -1 |           8 |       1.0000 |       1 |       0.00 |           0 |       1.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |             15 |                    8 |      37591 |            27270 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |             13 |                    8 |      37591 |            34596 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |              4 |                    1 |       1692 |             4001 |            -1 |          -1 |           6 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |             16 |                   16 |      37202 |            37202 |            -1 |          -1 |           2 |       1.0000 |       1 |       3.00 |           0 |       0.0 |          0 |         1.69 |          5.07 |        5.07 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |              1 |                    1 |       1692 |             1692 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         5.69 |          5.69 |        5.69 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |              8 |                    8 |      37591 |            37591 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         7.18 |          7.18 |        7.18 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |            999 |                  999 |         -1 |               -1 |            -1 |          -1 |          14 |       1.0000 |       1 |       0.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   1.61 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |             17 |                   17 |      37667 |            37667 |            -1 |          -1 |           2 |       1.0000 |       1 |       3.00 |           0 |       0.0 |          0 |         1.69 |          5.07 |        5.07 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |              7 |                    1 |       1692 |            27249 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434308 |   9196 |      1377 |    150215 |           2 |             1 |              2 |                    1 |       1692 |             3844 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |              4 |                    1 |      36021 |            35519 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |              8 |                    5 |      36021 |             2171 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |              2 |                    1 |      36021 |            35930 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |              6 |                    5 |      36021 |            35930 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |              3 |                    1 |      36021 |            35518 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |              1 |                    1 |      36021 |            36021 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         4.29 |          4.29 |        4.29 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |              9 |                    9 |      36989 |            36989 |            -1 |          -1 |           2 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.79 |          1.79 |        1.79 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |             10 |                   10 |      36935 |            36935 |            -1 |          -1 |           2 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         3.39 |          3.39 |        3.39 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |              7 |                    5 |      36021 |             2144 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |              5 |                    5 |      36021 |            36021 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         4.29 |          4.29 |        4.29 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |             11 |                   11 |        802 |              802 |            -1 |          -1 |           2 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.19 |          1.19 |        1.19 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   7008434492 |   9196 |       816 |    150216 |           2 |             3 |            999 |                  999 |         -1 |               -1 |            -1 |          -1 |          14 |       1.0000 |       1 |       0.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   1.05 |          1 |                              -1 |
+--------------+--------+-----------+-----------+-------------+---------------+----------------+----------------------+------------+------------------+---------------+-------------+-------------+--------------+---------+------------+-------------+-----------+------------+--------------+---------------+-------------+---------------+--------------+--------+------------+---------------------------------+
*/

-- compare records from quarterly table and master
select t.DW_GC_HEADER, t.DW_DAY, t.DW_RESTID, o.DW_OCCASION, o.OCCASIONDESC, t.DW_PRODUCT, p.PRODUCTDESC, t.DW_PRODUCTDETAIL, t.DW_PRODUCTMOD, t.DW_LINEITEM, l.LINEITEMDESC, t.DW_LINEITEMSEQ, t.DW_LINEITEMSEQPARENT, t.ACTQTYSOLD, t.ACTPRODPRICE, t.ACTGROSSSALES, t.ACTTAX
from TLD_FACT_2015_Q01 t
    left join LINEITEM_DIM l using (DW_LINEITEM)
    left join PRODUCT_DIM p using (DW_PRODUCT)
	left join OCCASION_DIM o using (DW_OCCASION)
where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492
order by DW_GC_HEADER, DW_PRODUCT;
/*
+--------------+--------+-----------+-------------+--------------+------------+--------------------------+------------------+---------------+-------------+-------------------------+----------------+----------------------+------------+--------------+---------------+--------+
| DW_GC_HEADER | DW_DAY | DW_RESTID | DW_OCCASION | OCCASIONDESC | DW_PRODUCT | PRODUCTDESC              | DW_PRODUCTDETAIL | DW_PRODUCTMOD | DW_LINEITEM | LINEITEMDESC            | DW_LINEITEMSEQ | DW_LINEITEMSEQPARENT | ACTQTYSOLD | ACTPRODPRICE | ACTGROSSSALES | ACTTAX |
+--------------+--------+-----------+-------------+--------------+------------+--------------------------+------------------+---------------+-------------+-------------------------+----------------+----------------------+------------+--------------+---------------+--------+
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |         -1 | N/A                      |               -1 |            -1 |          14 | TAX-LINE                |            999 |                  999 |       0.00 |         0.00 |          0.00 |   1.61 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |       1692 | COMBO 7                  |            27249 |            -1 |           3 | COMBO-DETAIL            |              7 |                    1 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |       1692 | COMBO 7                  |             3844 |            -1 |           3 | COMBO-DETAIL            |              2 |                    1 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |       1692 | COMBO 7                  |             4001 |            -1 |           6 | COMBO-DETAIL-REPLS-EQL  |              4 |                    1 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |       1692 | COMBO 7                  |             4001 |          2948 |           8 | COMBO-MOD-INGRD-MINUS   |              5 |                    4 |       0.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |       1692 | COMBO 7                  |             1692 |            -1 |           1 | COMBO-ITEM              |              1 |                    1 |       1.00 |         5.69 |          5.69 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |      37202 | CRWRP SLDR CHD           |            37202 |            -1 |           2 | NON-COMBO-ITEM          |             16 |                   16 |       3.00 |         1.69 |          5.07 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |      37591 | QUESARITO BOX            |            38194 |            -1 |           4 | COMBO-DETAIL-REPLS-PLUS |             10 |                    8 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |      37591 | QUESARITO BOX            |            37591 |            -1 |           1 | COMBO-ITEM              |              8 |                    8 |       1.00 |         7.18 |          7.18 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |      37591 | QUESARITO BOX            |            27270 |            -1 |           3 | COMBO-DETAIL            |             15 |                    8 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |      37591 | QUESARITO BOX            |            34596 |            -1 |           3 | COMBO-DETAIL            |             13 |                    8 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |      37591 | QUESARITO BOX            |             4001 |            -1 |           6 | COMBO-DETAIL-REPLS-EQL  |             12 |                    8 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434308 |   9196 |    150215 |           2 | DRIVE-THRU   |      37667 | CW SLDR SPICY SC         |            37667 |            -1 |           2 | NON-COMBO-ITEM          |             17 |                   17 |       3.00 |         1.69 |          5.07 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |         -1 | N/A                      |               -1 |            -1 |          14 | TAX-LINE                |            999 |                  999 |       0.00 |         0.00 |          0.00 |   1.05 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |        802 | BEAN BURRITO             |              802 |            -1 |           2 | NON-COMBO-ITEM          |             11 |                   11 |       1.00 |         1.19 |          1.19 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36021 | ROLLED CK 4-PK           |            35930 |            -1 |           3 | COMBO-DETAIL            |              6 |                    5 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36021 | ROLLED CK 4-PK           |            35930 |            -1 |           3 | COMBO-DETAIL            |              2 |                    1 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36021 | ROLLED CK 4-PK           |             2171 |            -1 |           3 | COMBO-DETAIL            |              8 |                    5 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36021 | ROLLED CK 4-PK           |            35519 |            -1 |           3 | COMBO-DETAIL            |              4 |                    1 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36021 | ROLLED CK 4-PK           |            36021 |            -1 |           1 | COMBO-ITEM              |              5 |                    5 |       1.00 |         4.29 |          4.29 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36021 | ROLLED CK 4-PK           |             2144 |            -1 |           3 | COMBO-DETAIL            |              7 |                    5 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36021 | ROLLED CK 4-PK           |            36021 |            -1 |           1 | COMBO-ITEM              |              1 |                    1 |       1.00 |         4.29 |          4.29 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36021 | ROLLED CK 4-PK           |            35518 |            -1 |           3 | COMBO-DETAIL            |              3 |                    1 |       1.00 |         0.00 |          0.00 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36935 | QUESARITO STEAK          |            36935 |            -1 |           2 | NON-COMBO-ITEM          |             10 |                   10 |       1.00 |         3.39 |          3.39 |   0.00 |
|   7008434492 |   9196 |    150216 |           2 | DRIVE-THRU   |      36989 | SHREDDED CHICKEN BURRITO |            36989 |            -1 |           2 | NON-COMBO-ITEM          |              9 |                    9 |       1.00 |         1.79 |          1.79 |   0.00 |
+--------------+--------+-----------+-------------+--------------+------------+--------------------------+------------------+---------------+-------------+-------------------------+----------------+----------------------+------------+--------------+---------------+--------+
*/

select t.DW_GC_HEADER, t.DW_DAY, t.DW_RESTID, o.DW_OCCASION, o.OCCASIONDESC, t.DW_PRODUCT, p.PRODUCTDESC, t.DW_PRODUCTDETAIL, t.DW_PRODUCTMOD, t.DW_LINEITEM, l.LINEITEMDESC, t.DW_LINEITEMSEQ, t.DW_LINEITEMSEQPARENT, t.ACTQTYSOLD, t.ACTPRODPRICE, t.ACTGROSSSALES, t.ACTTAX
from TLD_FACT t
    left join LINEITEM_DIM l using (DW_LINEITEM)
    left join PRODUCT_DIM p using (DW_PRODUCT)
	left join OCCASION_DIM o using (DW_OCCASION)
where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492
order by DW_GC_HEADER, DW_PRODUCT; -- table empty

-- tab number of transactions in master and quarterly tables
select
	(select count(*) from TLD_FACT) as total,
	(select count(*) from TLD_FACT_2007_Q01) as 07q1,
	(select count(*) from TLD_FACT_2007_Q02) as 07q2,
	(select count(*) from TLD_FACT_2007_Q03) as 07q3,
	(select count(*) from TLD_FACT_2007_Q04) as 07q4,
	(select count(*) from TLD_FACT_2008_Q01) as 08q1,
	(select count(*) from TLD_FACT_2008_Q02) as 08q2,
	(select count(*) from TLD_FACT_2008_Q03) as 08q3,
	(select count(*) from TLD_FACT_2008_Q04) as 08q4,
	(select count(*) from TLD_FACT_2009_Q01) as 09q1,
	(select count(*) from TLD_FACT_2009_Q02) as 09q2,
	(select count(*) from TLD_FACT_2009_Q03) as 09q3,
	(select count(*) from TLD_FACT_2009_Q04) as 09q4,
	(select count(*) from TLD_FACT_2010_Q01) as 10q1,
	(select count(*) from TLD_FACT_2010_Q02) as 10q2,
	(select count(*) from TLD_FACT_2010_Q03) as 10q3,
	(select count(*) from TLD_FACT_2010_Q04) as 10q4,
	(select count(*) from TLD_FACT_2011_Q01) as 11q1,
	(select count(*) from TLD_FACT_2011_Q02) as 11q2,
	(select count(*) from TLD_FACT_2011_Q03) as 11q3,
	(select count(*) from TLD_FACT_2011_Q04) as 11q4,
	(select count(*) from TLD_FACT_2012_Q01) as 12q1,
	(select count(*) from TLD_FACT_2012_Q02) as 12q2,
	(select count(*) from TLD_FACT_2012_Q03) as 12q3,
	(select count(*) from TLD_FACT_2012_Q04) as 12q4,
	(select count(*) from TLD_FACT_2013_Q01) as 13q1,
	(select count(*) from TLD_FACT_2013_Q02) as 13q2,
	(select count(*) from TLD_FACT_2013_Q03) as 13q3,
	(select count(*) from TLD_FACT_2013_Q04) as 13q4,
	(select count(*) from TLD_FACT_2014_Q01) as 14q1,
	(select count(*) from TLD_FACT_2014_Q02) as 14q2,
	(select count(*) from TLD_FACT_2014_Q03) as 14q3,
	(select count(*) from TLD_FACT_2014_Q04) as 14q4,
	(select count(*) from TLD_FACT_2015_Q01) as 15q1,
	(select count(*) from TLD_FACT_2015_Q02) as 15q2,
	(select count(*) from TLD_FACT_2015_Q03) as 15q3;
/*
+------------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+------------+------------+------------+------------+------------+-----------+------------+------------+------------+------------+------------+------------+
| total      | 07q1      | 07q2      | 07q3      | 07q4      | 08q1      | 08q2      | 08q3      | 08q4      | 09q1      | 09q2      | 09q3      | 09q4      | 10q1      | 10q2      | 10q3      | 10q4      | 11q1      | 11q2      | 11q3      | 11q4      | 12q1      | 12q2      | 12q3      | 12q4       | 13q1       | 13q2       | 13q3       | 13q4       | 14q1      | 14q2       | 14q3       | 14q4       | 15q1       | 15q2       | 15q3       |
+------------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+------------+------------+------------+------------+------------+-----------+------------+------------+------------+------------+------------+------------+
| 4965182657 | 727957470 | 410591488 | 408919477 | 525394470 | 393761041 | 436986687 | 465021041 | 608088041 | 414278381 | 456293970 | 461288824 | 794410882 | 652860758 | 702903894 | 722179580 | 875304412 | 584204423 | 613618156 | 604264985 | 810326023 | 605991280 | 818032696 | 940370314 | 1239434138 | 1001924881 | 1152821407 | 1240517750 | 1556274400 | 806956251 | 1227420117 | 1240061145 | 1651024877 | 1207030560 | 1325307922 | 1353794936 |
+------------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+------------+------------+------------+------------+------------+-----------+------------+------------+------------+------------+------------+------------+
*/	

select
	(select count(*) from TLD_FACT where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as total,
	(select count(*) from TLD_FACT_2007_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 07q1,
	(select count(*) from TLD_FACT_2007_Q02 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 07q2,
	(select count(*) from TLD_FACT_2007_Q03 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 07q3,
	(select count(*) from TLD_FACT_2007_Q04 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 07q4,
	(select count(*) from TLD_FACT_2008_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 08q1,
	(select count(*) from TLD_FACT_2008_Q02 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 08q2,
	(select count(*) from TLD_FACT_2008_Q03 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 08q3,
	(select count(*) from TLD_FACT_2008_Q04 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 08q4,
	(select count(*) from TLD_FACT_2009_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 09q1,
	(select count(*) from TLD_FACT_2009_Q02 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 09q2,
	(select count(*) from TLD_FACT_2009_Q03 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 09q3,
	(select count(*) from TLD_FACT_2009_Q04 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 09q4,
	(select count(*) from TLD_FACT_2010_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 10q1,
	(select count(*) from TLD_FACT_2010_Q02 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 10q2,
	(select count(*) from TLD_FACT_2010_Q03 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 10q3,
	(select count(*) from TLD_FACT_2010_Q04 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 10q4,
	(select count(*) from TLD_FACT_2011_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 11q1,
	(select count(*) from TLD_FACT_2011_Q02 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 11q2,
	(select count(*) from TLD_FACT_2011_Q03 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 11q3,
	(select count(*) from TLD_FACT_2011_Q04 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 11q4,
	(select count(*) from TLD_FACT_2012_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 12q1,
	(select count(*) from TLD_FACT_2012_Q02 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 12q2,
	(select count(*) from TLD_FACT_2012_Q03 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 12q3,
	(select count(*) from TLD_FACT_2012_Q04 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 12q4,
	(select count(*) from TLD_FACT_2013_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 13q1,
	(select count(*) from TLD_FACT_2013_Q02 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 13q2,
	(select count(*) from TLD_FACT_2013_Q03 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 13q3,
	(select count(*) from TLD_FACT_2013_Q04 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 13q4,
	(select count(*) from TLD_FACT_2014_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 14q1,
	(select count(*) from TLD_FACT_2014_Q02 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 14q2,
	(select count(*) from TLD_FACT_2014_Q03 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 14q3,
	(select count(*) from TLD_FACT_2014_Q04 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 14q4,
	(select count(*) from TLD_FACT_2015_Q01 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 15q1,
	(select count(*) from TLD_FACT_2015_Q02 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 15q2,
	(select count(*) from TLD_FACT_2015_Q03 where DW_GC_HEADER=7008434308 or DW_GC_HEADER=7008434492) as 15q3;
/* only available in 15Q1
+-------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+
| total | 07q1 | 07q2 | 07q3 | 07q4 | 08q1 | 08q2 | 08q3 | 08q4 | 09q1 | 09q2 | 09q3 | 09q4 | 10q1 | 10q2 | 10q3 | 10q4 | 11q1 | 11q2 | 11q3 | 11q4 | 12q1 | 12q2 | 12q3 | 12q4 | 13q1 | 13q2 | 13q3 | 13q4 | 14q1 | 14q2 | 14q3 | 14q4 | 15q1 | 15q2 | 15q3 |
+-------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+
|     0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |   25 |    0 |    0 |
+-------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+------+
1 row in set (1 hour 54 min 23.65 sec)
*/

select count(distinct DW_GC_HEADER) from TLD_FACT;

-- check what transactions from quarterly tables are also available in master
select count(*) from GC_HEADER_DIM_2010_Q01;

select
	/*(select DW_GC_HEADER from TLD_FACT) as total,*/
	(select DW_GC_HEADER from TLD_FACT_2007_Q01 limit 1) as 07q1,
	(select DW_GC_HEADER from TLD_FACT_2007_Q02 limit 1) as 07q2,
	(select DW_GC_HEADER from TLD_FACT_2007_Q03 limit 1) as 07q3,
	(select DW_GC_HEADER from TLD_FACT_2007_Q04 limit 1) as 07q4,
	(select DW_GC_HEADER from TLD_FACT_2008_Q01 limit 1) as 08q1,
	(select DW_GC_HEADER from TLD_FACT_2008_Q02 limit 1) as 08q2,
	(select DW_GC_HEADER from TLD_FACT_2008_Q03 limit 1) as 08q3,
	(select DW_GC_HEADER from TLD_FACT_2008_Q04 limit 1) as 08q4,
	(select DW_GC_HEADER from TLD_FACT_2009_Q01 limit 1) as 09q1,
	(select DW_GC_HEADER from TLD_FACT_2009_Q02 limit 1) as 09q2,
	(select DW_GC_HEADER from TLD_FACT_2009_Q03 limit 1) as 09q3,
	(select DW_GC_HEADER from TLD_FACT_2009_Q04 limit 1) as 09q4,
	(select DW_GC_HEADER from TLD_FACT_2010_Q01 limit 1) as 10q1,
	(select DW_GC_HEADER from TLD_FACT_2010_Q02 limit 1) as 10q2,
	(select DW_GC_HEADER from TLD_FACT_2010_Q03 limit 1) as 10q3,
	(select DW_GC_HEADER from TLD_FACT_2010_Q04 limit 1) as 10q4,
	(select DW_GC_HEADER from TLD_FACT_2011_Q01 limit 1) as 11q1,
	(select DW_GC_HEADER from TLD_FACT_2011_Q02 limit 1) as 11q2,
	(select DW_GC_HEADER from TLD_FACT_2011_Q03 limit 1) as 11q3,
	(select DW_GC_HEADER from TLD_FACT_2011_Q04 limit 1) as 11q4,
	(select DW_GC_HEADER from TLD_FACT_2012_Q01 limit 1) as 12q1,
	(select DW_GC_HEADER from TLD_FACT_2012_Q02 limit 1) as 12q2,
	(select DW_GC_HEADER from TLD_FACT_2012_Q03 limit 1) as 12q3,
	(select DW_GC_HEADER from TLD_FACT_2012_Q04 limit 1) as 12q4,
	(select DW_GC_HEADER from TLD_FACT_2013_Q01 limit 1) as 13q1,
	(select DW_GC_HEADER from TLD_FACT_2013_Q02 limit 1) as 13q2,
	(select DW_GC_HEADER from TLD_FACT_2013_Q03 limit 1) as 13q3,
	(select DW_GC_HEADER from TLD_FACT_2013_Q04 limit 1) as 13q4,
	(select DW_GC_HEADER from TLD_FACT_2014_Q01 limit 1) as 14q1,
	(select DW_GC_HEADER from TLD_FACT_2014_Q02 limit 1) as 14q2,
	(select DW_GC_HEADER from TLD_FACT_2014_Q03 limit 1) as 14q3,
	(select DW_GC_HEADER from TLD_FACT_2014_Q04 limit 1) as 14q4,
	(select DW_GC_HEADER from TLD_FACT_2015_Q01 limit 1) as 15q1,
	(select DW_GC_HEADER from TLD_FACT_2015_Q02 limit 1) as 15q2,
	(select DW_GC_HEADER from TLD_FACT_2015_Q03 limit 1) as 15q3;
/*
+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+
| 07q1       | 07q2       | 07q3       | 07q4       | 08q1       | 08q2       | 08q3       | 08q4       | 09q1       | 09q2       | 09q3       | 09q4       | 10q1       | 10q2       | 10q3       | 10q4       | 11q1       | 11q2       | 11q3       | 11q4       | 12q1       | 12q2       | 12q3       | 12q4       | 13q1       | 13q2       | 13q3       | 13q4       | 14q1       | 14q2       | 14q3       | 14q4       | 15q1       | 15q2       | 15q3       |
+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+
| 2193837694 | 2268477652 | 2370598056 | 2440911143 | 2531798051 | 2600038097 | 2675062530 | 2757114830 | 2864539602 | 2938503743 | 3020978631 | 3104426534 | 3252234169 | 3370415535 | 3499666135 | 3625863678 | 3782963763 | 3886539517 | 3995506502 | 4106212179 | 4253655046 | 4367247208 | 4519622138 | 4693969274 | 5034512599 | 5171371477 | 5430925256 | 5748548201 | 5833189728 | 6045008582 | 6438545377 | 6552786309 | 7008429708 | 7290376505 | 7452761975 |
+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+------------+
1 row in set (0.56 sec)
*/

select
	(select count(*) from TLD_FACT where DW_GC_HEADER=2193837694) as 07q1, --10
	(select count(*) from TLD_FACT where DW_GC_HEADER=2268477652) as 07q2, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=2370598056) as 07q3, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=2440911143) as 07q4; --0
select
	(select count(*) from TLD_FACT where DW_GC_HEADER=2531798051) as 08q1, --2
	(select count(*) from TLD_FACT where DW_GC_HEADER=2600038097) as 08q2, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=2675062530) as 08q3, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=2757114830) as 08q4; --0
select
	(select count(*) from TLD_FACT where DW_GC_HEADER=2864539602) as 09q1, --10
	(select count(*) from TLD_FACT where DW_GC_HEADER=2938503743) as 09q2, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=3020978631) as 09q3, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=3104426534) as 09q4; --0
select
	(select count(*) from TLD_FACT where DW_GC_HEADER=3252234169) as 10q1, --7
	(select count(*) from TLD_FACT where DW_GC_HEADER=3370415535) as 10q2, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=3499666135) as 10q3, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=3625863678) as 10q4; --0
select
	(select count(*) from TLD_FACT where DW_GC_HEADER=3782963763) as 11q1, --16
	(select count(*) from TLD_FACT where DW_GC_HEADER=3886539517) as 11q2, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=3995506502) as 11q3, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=4106212179) as 11q4; --0
select
	(select count(*) from TLD_FACT where DW_GC_HEADER=4253655046) as 12q1, --4
	(select count(*) from TLD_FACT where DW_GC_HEADER=4367247208) as 12q2, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=4519622138) as 12q3, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=4693969274) as 12q4; --0
select
	(select count(*) from TLD_FACT where DW_GC_HEADER=5034512599) as 13q1, --3
	(select count(*) from TLD_FACT where DW_GC_HEADER=5171371477) as 13q2, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=5430925256) as 13q3, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=5748548201) as 13q4; --0
select
	(select count(*) from TLD_FACT where DW_GC_HEADER=5833189728) as 14q1, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=6045008582) as 14q2, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=6438545377) as 14q3, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=6552786309) as 14q4; --0
select
	(select count(*) from TLD_FACT where DW_GC_HEADER=7008429708) as 15q1, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=7290376505) as 15q2, --0
	(select count(*) from TLD_FACT where DW_GC_HEADER=7452761975) as 15q3; --0

-- descriptives
select AGREEMENT_CD, count(*)
from ALIGN_DIM
where CLOSEDDT>="2007-01-01" or CLOSEDDT="0000-00-00"
group by AGREEMENT_CD;

/*
* NEW PRODCT ADDED BY TLD *
FRANCHISE LOCAL MENU
PIZZA HUT
KFC
LJS/LIS?
PHI
AWR
*/
select t.DW_PRODUCT, p.PRODUCTDESC, count(*)
from TLD_FACT_2007_Q01 t
	left join PRODUCT_DIM p using(DW_PRODUCT)
where PRODUCTDESC like ("%* NEW PRODCT ADDED BY TLD *%")
	or PRODUCTDESC like ("%FRANCHISE LOCAL MENU%")
	or PRODUCTDESC like ("KFC%")
	or PRODUCTDESC like ("PIZZA HUT%")
	or PRODUCTDESC like ("PHI%")
	or PRODUCTDESC like ("LJS%")
	or PRODUCTDESC like ("AWR%")
	or PRODUCTDESC like ("%COMBO%")
group by PRODUCTDESC;

--- group by productdesc
select (select count(*) from TLD_FACT_2014_Q01 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%* NEW PRODCT ADDED BY TLD *%")) as newproduct;
select (select count(*) from TLD_FACT_2014_Q01 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%FRANCHISE LOCAL MENU%")) as franchise;
select (select count(*) from TLD_FACT_2014_Q01 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%KFC%") or PRODUCTDESC like ("%PIZZA HUT%") or PRODUCTDESC like ("%PHI%") or PRODUCTDESC like ("%LJS%") or PRODUCTDESC like ("%AWR%")) as otherbrand;

select (select count(*) from TLD_FACT_2014_Q02 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%* NEW PRODCT ADDED BY TLD *%")) as newproduct;
select (select count(*) from TLD_FACT_2014_Q02 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%FRANCHISE LOCAL MENU%")) as franchise;
select (select count(*) from TLD_FACT_2014_Q02 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%KFC%") or PRODUCTDESC like ("%PIZZA HUT%") or PRODUCTDESC like ("%PHI%") or PRODUCTDESC like ("%LJS%") or PRODUCTDESC like ("%AWR%")) as otherbrand;

select (select count(*) from TLD_FACT_2014_Q03 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%* NEW PRODCT ADDED BY TLD *%")) as newproduct;
select (select count(*) from TLD_FACT_2014_Q03 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%FRANCHISE LOCAL MENU%")) as franchise;
select (select count(*) from TLD_FACT_2014_Q03 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%KFC%") or PRODUCTDESC like ("%PIZZA HUT%") or PRODUCTDESC like ("%PHI%") or PRODUCTDESC like ("%LJS%") or PRODUCTDESC like ("%AWR%")) as otherbrand;

select (select count(*) from TLD_FACT_2014_Q04 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%* NEW PRODCT ADDED BY TLD *%")) as newproduct;
select (select count(*) from TLD_FACT_2014_Q04 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%FRANCHISE LOCAL MENU%")) as franchise;
select (select count(*) from TLD_FACT_2014_Q04 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC like ("%KFC%") or PRODUCTDESC like ("%PIZZA HUT%") or PRODUCTDESC like ("%PHI%") or PRODUCTDESC like ("%LJS%") or PRODUCTDESC like ("%AWR%")) as otherbrand;

--select (select count(*) from TLD_FACT_2013_Q04 left join PRODUCT_DIM using(DW_PRODUCT) where (PRODUCTDESC like ("%COMBO 1%") or PRODUCTDESC like ("%COMBO 2%") or PRODUCTDESC like ("%COMBO 3%") or PRODUCTDESC like ("%COMBO 4%") or PRODUCTDESC like ("%COMBO 5%") or PRODUCTDESC like ("%COMBO 6%") or PRODUCTDESC like ("%COMBO 7%") or PRODUCTDESC like ("%COMBO 8%") or PRODUCTDESC like ("%COMBO 9%") or PRODUCTDESC like ("%COMBO #10%") or PRODUCTDESC like ("%COMBO 10%") or PRODUCTDESC like ("%COMBO 11%")) and PRODUCTDESC not like ("%KFC%") and PRODUCTDESC not like ("%PHI%") and PRODUCTDESC not like ("%PIZZA HUT%") and PRODUCTDESC not like ("%LJS%") and PRODUCTDESC not like ("%AWR%")) as combo;

-- 
select p.PRODUCTDESC, g.PRODUCTGROUPDESC
from PRODUCT_DIM p
	left join PRODUCT_GROUP_DET g using (DW_PRODUCTGROUP)
where PRODUCTGROUPDESC!="KFC" 
	and PRODUCTGROUPDESC!="PIZZA HUT" 
	and PRODUCTGROUPDESC!="UNASSIGNED"
	and PRODUCTDESC like ("%COMBO %")
order by DW_PRODUCT
limit 50;



-- characterize purchase type, dine-in/takeout/drive-thru, by quarter/year
select DW_OCCASION, count(*) from TLD_FACT_2014_Q03 group by DW_OCCASION;

-- characterize purchase time, late night/breakfast/lunch/afternoon/dinner/evening
select t.DW_DAYPART, count(*) from TLD_FACT_2015_Q01 left join TIME_MINUTE_DIM t using(DW_MINUTE) group by DW_DAYPART;

select t.DW_PRODUCT, g.PRODUCTGROUPDESC, p.PRODUCTDESC, t.DW_GC_HEADER
FROM TLD_FACT_2007_Q02 t
left join PRODUCT_DIM p using(DW_PRODUCT)
left join PRODUCT_GROUP_DET g using(DW_PRODUCTGROUP)
where DW_PRODUCT=1769
limit 20;
/*
+------------+------------------+-------------+--------------+
| DW_PRODUCT | PRODUCTGROUPDESC | PRODUCTDESC | DW_GC_HEADER |
+------------+------------------+-------------+--------------+
|       1769 | COMBOS           | COMBO 8     |   2268477514 |
|       1769 | COMBOS           | COMBO 8     |   2268477514 |
|       1769 | COMBOS           | COMBO 8     |   2268477514 |
|       1769 | COMBOS           | COMBO 8     |   2268484138 |
|       1769 | COMBOS           | COMBO 8     |   2268484138 |
|       1769 | COMBOS           | COMBO 8     |   2268484138 |
|       1769 | COMBOS           | COMBO 8     |   2268486944 |
|       1769 | COMBOS           | COMBO 8     |   2268486944 |
|       1769 | COMBOS           | COMBO 8     |   2268486944 |
|       1769 | COMBOS           | COMBO 8     |   2268486944 |
|       1769 | COMBOS           | COMBO 8     |   2268893456 |
|       1769 | COMBOS           | COMBO 8     |   2268894238 |
|       1769 | COMBOS           | COMBO 8     |   2268893456 |
|       1769 | COMBOS           | COMBO 8     |   2268894238 |
|       1769 | COMBOS           | COMBO 8     |   2268893456 |
|       1769 | COMBOS           | COMBO 8     |   2268894238 |
|       1769 | COMBOS           | COMBO 8     |   2268893456 |
|       1769 | COMBOS           | COMBO 8     |   2268894238 |
|       1769 | COMBOS           | COMBO 8     |   2268396382 |
|       1769 | COMBOS           | COMBO 8     |   2268396382 |
+------------+------------------+-------------+--------------+
*/

/*
+--------------+--------+-----------+-----------+-------------+---------------+----------------+----------------------+------------+------------------+---------------+-------------+-------------+--------------+---------+------------+-------------+-----------+------------+--------------+---------------+-------------+---------------+--------------+--------+------------+---------------------------------+
| DW_GC_HEADER | DW_DAY | DW_MINUTE | DW_RESTID | DW_OCCASION | DW_TENDERTYPE | DW_LINEITEMSEQ | DW_LINEITEMSEQPARENT | DW_PRODUCT | DW_PRODUCTDETAIL | DW_PRODUCTMOD | DW_DISCOUNT | DW_LINEITEM | SEASONFACTOR | COUNTER | ACTQTYSOLD | ACTPROMOQTY | ACTMODQTY | ACTDISCQTY | ACTPRODPRICE | ACTGROSSSALES | ACTNETSALES | ACTPROMOSALES | ACTDISCSALES | ACTTAX | DW_CHANNEL | DW_CHECK_IN_LOCATION_IDENTIFIER |
+--------------+--------+-----------+-----------+-------------+---------------+----------------+----------------------+------------+------------------+---------------+-------------+-------------+--------------+---------+------------+-------------+-----------+------------+--------------+---------------+-------------+---------------+--------------+--------+------------+---------------------------------+
|   2268477514 |   6294 |       723 |     31704 |           2 |             1 |              8 |                    8 |         -1 |               -1 |            -1 |          -1 |          14 |       1.0000 |       1 |       0.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.46 |          1 |                              -1 |
|   2268477514 |   6294 |       723 |     31704 |           2 |             1 |              6 |                    4 |        802 |              802 |          2549 |          -1 |          10 |       1.0000 |       1 |       0.00 |           0 |       1.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   2268477514 |   6294 |       723 |     31704 |           2 |             1 |              5 |                    4 |        802 |              802 |          3903 |          -1 |          11 |       1.0000 |       1 |       0.00 |           0 |       1.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   2268477514 |   6294 |       723 |     31704 |           2 |             1 |              4 |                    4 |        802 |              802 |            -1 |          -1 |           2 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.89 |          0.89 |        0.89 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   2268477514 |   6294 |       723 |     31704 |           2 |             1 |              2 |                    1 |       1769 |             3649 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   2268477514 |   6294 |       723 |     31704 |           2 |             1 |              3 |                    1 |       1769 |             4450 |            -1 |          -1 |           3 |       1.0000 |       1 |       3.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   2268477514 |   6294 |       723 |     31704 |           2 |             1 |              1 |                    1 |       1769 |             1769 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         3.49 |          3.49 |        3.49 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
|   2268477514 |   6294 |       723 |     31704 |           2 |             1 |              7 |                    7 |       3213 |             3213 |            -1 |          -1 |           2 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.19 |          1.19 |        1.19 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |
+--------------+--------+-----------+-----------+-------------+---------------+----------------+----------------------+------------+------------------+---------------+-------------+-------------+--------------+---------+------------+-------------+-----------+------------+--------------+---------------+-------------+---------------+--------------+--------+------------+---------------------------------+
*/

/*
+------------+--------------+--------+-----------+-----------+-------------+---------------+----------------+----------------------+------------------+---------------+-------------+-------------+--------------+---------+------------+-------------+-----------+------------+--------------+---------------+-------------+---------------+--------------+--------+------------+---------------------------------+-----------------+-----------+----------------------+-----------------+---------------------+---------------------+----------------+
| DW_PRODUCT | DW_GC_HEADER | DW_DAY | DW_MINUTE | DW_RESTID | DW_OCCASION | DW_TENDERTYPE | DW_LINEITEMSEQ | DW_LINEITEMSEQPARENT | DW_PRODUCTDETAIL | DW_PRODUCTMOD | DW_DISCOUNT | DW_LINEITEM | SEASONFACTOR | COUNTER | ACTQTYSOLD | ACTPROMOQTY | ACTMODQTY | ACTDISCQTY | ACTPRODPRICE | ACTGROSSSALES | ACTNETSALES | ACTPROMOSALES | ACTDISCSALES | ACTTAX | DW_CHANNEL | DW_CHECK_IN_LOCATION_IDENTIFIER | DW_PRODUCTGROUP | PRODUCTCD | PRODUCTDESC          | PRODUCTSTATUSCD | PRODUCTSTATUSDT     | LASTUPDTDT          | LASTUPDTUSERID |
+------------+--------------+--------+-----------+-----------+-------------+---------------+----------------+----------------------+------------------+---------------+-------------+-------------+--------------+---------+------------+-------------+-----------+------------+--------------+---------------+-------------+---------------+--------------+--------+------------+---------------------------------+-----------------+-----------+----------------------+-----------------+---------------------+---------------------+----------------+
|      36010 |   4930703266 |   8403 |       857 |     41472 |           2 |             1 |              2 |                    2 |            36010 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.99 |          1.99 |        1.99 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | LRG DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      36010 |   4930703266 |   8403 |       857 |     41472 |           2 |             1 |              1 |                    1 |             3649 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | LRG DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      36011 |   4930705428 |   8403 |      1133 |     41472 |           3 |             1 |              1 |                    1 |             3648 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | MED DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      36011 |   4930705428 |   8403 |      1133 |     41472 |           3 |             1 |              2 |                    1 |            36011 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.79 |          1.79 |        1.79 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | MED DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      30164 |   4926074236 |   8401 |      1131 |     32205 |           2 |             3 |              6 |                    6 |            30164 |            -1 |          -1 |           2 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.00 |          1.00 |        1.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | 3TWST     | FRANCHISE LOCAL MENU | D               | 2009-11-11 21:00:18 | 2009-11-11 08:50:38 | DWT014         |
|      30164 |   4926076904 |   8401 |      1107 |     32319 |           2 |             3 |              2 |                    2 |            30164 |            -1 |          -1 |           2 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.00 |          1.00 |        1.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | 3TWST     | FRANCHISE LOCAL MENU | D               | 2009-11-11 21:00:18 | 2009-11-11 08:50:38 | DWT014         |
|      30164 |   4926076720 |   8401 |      1079 |     32319 |           3 |             1 |              1 |                    1 |            30164 |            -1 |          -1 |           2 |       1.0000 |       1 |       2.00 |           0 |       0.0 |          0 |         1.00 |          2.00 |        2.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | 3TWST     | FRANCHISE LOCAL MENU | D               | 2009-11-11 21:00:18 | 2009-11-11 08:50:38 | DWT014         |
|       6177 |   4926070449 |   8401 |       803 |     31901 |           3 |             1 |              1 |                    1 |             2098 |            -1 |          -1 |           6 |       1.0000 |       1 |       2.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | GRANDE    | FRANCHISE LOCAL MENU | D               | 2003-09-17 12:30:14 | 2003-09-16 18:47:22 | DWT014         |
|       6177 |   4926070449 |   8401 |       803 |     31901 |           3 |             1 |              2 |                    1 |             2100 |            -1 |          -1 |           3 |       1.0000 |       1 |       6.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | GRANDE    | FRANCHISE LOCAL MENU | D               | 2003-09-17 12:30:14 | 2003-09-16 18:47:22 | DWT014         |
|       6177 |   4926070449 |   8401 |       803 |     31901 |           3 |             1 |              1 |                    1 |             2094 |            -1 |          -1 |           6 |       1.0000 |       1 |       2.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | GRANDE    | FRANCHISE LOCAL MENU | D               | 2003-09-17 12:30:14 | 2003-09-16 18:47:22 | DWT014         |
|       6177 |   4926070449 |   8401 |       803 |     31901 |           3 |             1 |              3 |                    1 |             6177 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |        10.49 |         10.49 |       10.49 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | GRANDE    | FRANCHISE LOCAL MENU | D               | 2003-09-17 12:30:14 | 2003-09-16 18:47:22 | DWT014         |
|      30164 |   4975518813 |   8424 |       917 |     33627 |           2 |             3 |              4 |                    4 |            30164 |            -1 |          -1 |           2 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.00 |          1.00 |        1.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | 3TWST     | FRANCHISE LOCAL MENU | D               | 2009-11-11 21:00:18 | 2009-11-11 08:50:38 | DWT014         |
|      30164 |   4975518399 |   8424 |      1114 |     33627 |           1 |             1 |              1 |                    1 |            30164 |            -1 |          -1 |           2 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.00 |          1.00 |        1.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | 3TWST     | FRANCHISE LOCAL MENU | D               | 2009-11-11 21:00:18 | 2009-11-11 08:50:38 | DWT014         |
|      36011 |   4926077494 |   8401 |       867 |     32715 |           3 |             1 |              4 |                    3 |            36011 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.79 |          1.79 |        1.79 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | MED DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      36011 |   4926076344 |   8401 |      1035 |     32715 |           2 |             3 |              6 |                    5 |            36011 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.79 |          1.79 |        1.79 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | MED DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      36011 |   4926077494 |   8401 |       867 |     32715 |           3 |             1 |              3 |                    3 |             3648 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | MED DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      36011 |   4926076344 |   8401 |      1035 |     32715 |           2 |             3 |              5 |                    5 |             3648 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | MED DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      36011 |   4926079886 |   8401 |       804 |     32715 |           3 |             1 |              2 |                    2 |             3648 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | MED DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      36011 |   4926079886 |   8401 |       804 |     32715 |           3 |             1 |              3 |                    2 |            36011 |            -1 |          -1 |           1 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         1.79 |          1.79 |        1.79 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | MED DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
|      36012 |   4926078736 |   8401 |       924 |     32715 |           1 |             1 |              2 |                    2 |             3647 |            -1 |          -1 |           3 |       1.0000 |       1 |       1.00 |           0 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |          0.00 |         0.00 |   0.00 |          1 |                              -1 |              35 | SML DNK   | FRANCHISE LOCAL MENU | D               | 2012-09-07 21:00:12 | 2012-09-07 08:33:20 | DWT014         |
+------------+--------------+--------+-----------+-----------+-------------+---------------+----------------+----------------------+------------------+---------------+-------------+-------------+--------------+---------+------------+-------------+-----------+------------+--------------+---------------+-------------+---------------+--------------+--------+------------+---------------------------------+-----------------+-----------+----------------------+-----------------+---------------------+---------------------+----------------+
*/

SELECT t.DW_GC_HEADER, t.DW_PRODUCT, t.DW_PRODUCTDETAIL, p.PRODUCTDESC, t.DW_PRODUCTMOD, t. ACTQTYSOLD, t.ACTMODQTY, t.ACTDISCQTY, t.ACTPRODPRICE, t.ACTGROSSSALES, t.ACTNETSALES, t.ACTTAX
from TLD_FACT_2013_Q01 t
	left join PRODUCT_DIM p using(DW_PRODUCT) 
where DW_GC_HEADER=5047855937
order by DW_GC_HEADER, DW_PRODUCT;
/*
+--------------+------------+------------------+----------------------+---------------+------------+-----------+------------+--------------+---------------+-------------+--------+
| DW_GC_HEADER | DW_PRODUCT | DW_PRODUCTDETAIL | PRODUCTDESC          | DW_PRODUCTMOD | ACTQTYSOLD | ACTMODQTY | ACTDISCQTY | ACTPRODPRICE | ACTGROSSSALES | ACTNETSALES | ACTTAX |
+--------------+------------+------------------+----------------------+---------------+------------+-----------+------------+--------------+---------------+-------------+--------+
|   5047855937 |         -1 |               -1 | N/A                  |            -1 |       0.00 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |   0.52 |
|   5047855937 |       1690 |             3365 | COMBO 5              |            -1 |       1.00 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |   0.00 |
|   5047855937 |       1690 |             4005 | COMBO 5              |          4271 |       0.00 |       1.0 |          0 |         0.00 |          0.00 |        0.00 |   0.00 |
|   5047855937 |       1690 |             3649 | COMBO 5              |            -1 |       1.00 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |   0.00 |
|   5047855937 |       1690 |             4005 | COMBO 5              |          2948 |       0.00 |       1.0 |          0 |         0.00 |          0.00 |        0.00 |   0.00 |
|   5047855937 |       1690 |             4005 | COMBO 5              |            -1 |       1.00 |       0.0 |          0 |         0.00 |          0.00 |        0.00 |   0.00 |
|   5047855937 |       1690 |             4005 | COMBO 5              |          4582 |       0.00 |       1.0 |          0 |         0.00 |          0.00 |        0.00 |   0.00 |
|   5047855937 |       1690 |             1690 | COMBO 5              |            -1 |       1.00 |       0.0 |          0 |         5.49 |          5.49 |        5.49 |   0.00 |
|   5047855937 |       5528 |             5528 | FRANCHISE LOCAL MENU |            -1 |       1.00 |       0.0 |          0 |         1.59 |          1.59 |        1.59 |   0.00 |
|   5047855937 |       5528 |             5528 | FRANCHISE LOCAL MENU |          2948 |       0.00 |       1.0 |          0 |         0.00 |          0.00 |        0.00 |   0.00 |
+--------------+------------+------------------+----------------------+---------------+------------+-----------+------------+--------------+---------------+-------------+--------+
*/

-- transaction by payment type
select t.TENDERTYPEDESC, count(*) from GC_HEADER_DIM_2012_Q01 left join TENDERTYPE_DIM t using(DW_TENDERTYPE) group by TENDERTYPEDESC;

-- transaction dollar amount, by meal time
select t.DW_DAYPART, count(*) from GC_HEADER_DIM_2015_Q03 g left join TIME_MINUTE_DIM t using(DW_MINUTE) group by DW_DAYPART;

-- transaction amount, by weekday/weekend
select d.BUSIDAYNAME, count(*), sum(g.TOTGROSSSALES) from GC_HEADER_DIM_2015_Q03 g left join TIME_DAY_DIM d using(DW_DAY) group by BUSIDAYNAME order by DAYOFWEEK;

-- transaction amount, by occasion
select o.OCCASIONDESC, count(*), sum(g.TOTGROSSSALES) from GC_HEADER_DIM_2015_Q03 g left join OCCASION_DIM o using(DW_OCCASION) group by OCCASIONDESC order by DW_OCCASION;

-- fix 2012 Q3 numbers
select o.OCCASIONDESC, count(distinct DW_GC_HEADER), sum(t.ACTGROSSSALES) from TLD_FACT_2012_Q03 t left join OCCASION_DIM o using(DW_OCCASION) group by OCCASIONDESC order by DW_OCCASION;
select m.DW_DAYPART,  count(distinct DW_GC_HEADER), sum(t.ACTGROSSSALES) from TLD_FACT_2012_Q03 t left join TIME_MINUTE_DIM m using(DW_MINUTE) group by DW_DAYPART;
select d.BUSIDAYNAME, count(distinct DW_GC_HEADER), sum(t.ACTGROSSSALES) from TLD_FACT_2012_Q03 t left join TIME_DAY_DIM d using(DW_DAY) group by BUSIDAYNAME order by DAYOFWEEK;

-- figure out fiscal quarters
select FISCALQTRNO, FISCALQTRNAME, FISCALQTRBGNDT, FISCALQTRENDDT, QTRNO, QTRNAME, FISCALPRDNO, FISCALPRDNAME, FISCALPRDBGNDT, FISCALPRDENDDT  from TIME_DAY_DIM limit 50;
select distinct FISCALQTRNO, FISCALQTRBGNDT, FISCALQTRENDDT from TIME_DAY_DIM limit 50;
select distinct FISCALQTRNO, FISCALQTRBGNDT, FISCALQTRENDDT from TIME_DAY_DIM where FISCALQTRBGNDT>="2006-12-20" order by FISCALQTRNO into outfile '/gpfs/home/wue04/fiscal_dates.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
/* +-------------+----------------+----------------+
| FISCALQTRNO | FISCALQTRBGNDT | FISCALQTRENDDT |
+-------------+----------------+----------------+
| Y2007 Q01   | 2006-12-27     | 2007-03-20     |
| Y2007 Q02   | 2007-03-21     | 2007-06-12     |
| Y2007 Q03   | 2007-06-13     | 2007-09-04     |
| Y2007 Q04   | 2007-09-05     | 2007-12-25     |
| Y2008 Q01   | 2007-12-26     | 2008-03-18     |
| Y2008 Q02   | 2008-03-19     | 2008-06-10     |
| Y2008 Q03   | 2008-06-11     | 2008-09-02     |
| Y2008 Q04   | 2008-09-03     | 2008-12-23     |
| Y2009 Q01   | 2008-12-24     | 2009-03-17     |
| Y2009 Q02   | 2009-03-18     | 2009-06-09     |
| Y2009 Q03   | 2009-06-10     | 2009-09-01     |
| Y2009 Q04   | 2009-09-02     | 2009-12-22     |
| Y2010 Q01   | 2009-12-23     | 2010-03-16     |
| Y2010 Q02   | 2010-03-17     | 2010-06-08     |
| Y2010 Q03   | 2010-06-09     | 2010-08-31     |
| Y2010 Q04   | 2010-09-01     | 2010-12-21     |
| Y2011 Q01   | 2010-12-22     | 2011-03-15     |
| Y2011 Q02   | 2011-03-16     | 2011-06-07     |
| Y2011 Q03   | 2011-06-08     | 2011-08-30     |
| Y2011 Q04   | 2011-08-31     | 2011-12-27     |
| Y2012 Q01   | 2011-12-28     | 2012-03-20     |
| Y2012 Q02   | 2012-03-21     | 2012-06-12     |
| Y2012 Q03   | 2012-06-13     | 2012-09-04     |
| Y2012 Q04   | 2012-09-05     | 2012-12-25     |
| Y2013 Q01   | 2012-12-26     | 2013-03-19     |
| Y2013 Q02   | 2013-03-20     | 2013-06-11     |
| Y2013 Q03   | 2013-06-12     | 2013-09-03     |
| Y2013 Q04   | 2013-09-04     | 2013-12-24     |
| Y2014 Q01   | 2013-12-25     | 2014-03-18     |
| Y2014 Q02   | 2014-03-19     | 2014-06-10     |
| Y2014 Q03   | 2014-06-11     | 2014-09-02     |
| Y2014 Q04   | 2014-09-03     | 2014-12-23     |
| Y2015 Q01   | 2014-12-24     | 2015-03-17     |
| Y2015 Q02   | 2015-03-18     | 2015-06-09     |
| Y2015 Q03   | 2015-06-10     | 2015-09-01     |
| Y2015 Q04   | 2015-09-02     | 2015-12-22     |
+-------------+----------------+----------------+ */

--- export restaurants ownership, 2007-2011
select * from OWNERSHIP_2007_Q04 into outfile '/gpfs/home/wue04/ownership_2007_q04.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select AGREEMENTTYPE, RESTID from ALIGN_DIM into outfile '/gpfs/home/wue04/ownership.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

-- re-export restaurant file with ownership type to it
select DW_RESTID, STATUSDESC,  AGREEMENT_DESC, KFC_AGREEMENT_DESC,  TBC_AGREEMENT_DESC, PHI_AGREEMENT_DESC, LJS_AGREEMENT_DESC, AWR_AGREEMENT_DESC,
ADDRESS_LINE_1, ADDRESS_LINE_2, ADDRESS_LINE_3, CITYNAME, COUNTYNAME, STATENAME, PSTL_ZIP_CD,
OPENEDDT,  TEMPCLOSEDDT, REOPENDT, CLOSEDDT, LATITUDE, LONGITUDE, CONCEPTDESC, CONCEPTBEGINDT, CONCEPTENDDT,
DRIVETHRUIND, DRVTHU_TYPE from ALIGN_DIM into outfile '/gpfs/home/wue04/restaurants.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

--- by restaurant analysis
-- plot transaction volumn/dollar by restaurant/state and time
-- for restaurants with identical addresses, create new restaurant id
-- merge results based on DW_RESTID, but aggregate by new restaurant id
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2007_Q01 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2007q1.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2007_Q02 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2007q2.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2007_Q03 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2007q3.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2007_Q04 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2007q4.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2008_Q01 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2008q1.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2008_Q02 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2008q2.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2008_Q03 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2008q3.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2008_Q04 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2008q4.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2009_Q01 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2009q1.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2009_Q02 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2009q2.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2009_Q03 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2009q3.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2009_Q04 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2009q4.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2010_Q01 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2010q1.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2010_Q02 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2010q2.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2010_Q03 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2010q3.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2010_Q04 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2010q4.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2011_Q01 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2011q1.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2011_Q02 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2011q2.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2011_Q03 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2011q3.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2011_Q04 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2011q4.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2012_Q01 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2012q1.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2012_Q02 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2012q2.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2012_Q03 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2012q3.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2012_Q04 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2012q4.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2013_Q01 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2013q1.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2013_Q02 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2013q2.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2013_Q03 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2013q3.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2013_Q04 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2013q4.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2014_Q01 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2014q1.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2014_Q02 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2014q2.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2014_Q03 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2014q3.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2014_Q04 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2014q4.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2015_Q01 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2015q1.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2015_Q02 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2015q2.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';
select DW_RESTID, count(DW_GC_HEADER), sum(TOTGROSSSALES) from GC_HEADER_DIM_2015_Q03 group by DW_RESTID into outfile '/gpfs/home/wue04/tb-data/by_restaurant_transaction_2015q3.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';

--- sample query, create index
create unique index PRODUCT_DIM_DW_PRODUCT on PRODUCT_DIM (DW_PRODUCT);
create index TLD_FACT_2014_Q01_DW_PRODUCT on TLD_FACT_2014_Q01 (DW_PRODUCT);

select t.DW_GC_HEADER, p.PRODUCTDESC, d.PRODUCTDESC, m.PRODUCTDESC,
	t.DW_LINEITEM, l.LINEITEMDESC,
	t.ACTNETSALES, t.ACTPRODPRICE, t.ACTQTYSOLD
	from TLD_FACT_2007_Q02 t
		left join PRODUCT_DETAIL_DIM_V1 d using(DW_PRODUCTDETAIL)
		left join PRODUCT_DIM p using(DW_PRODUCT)
		left join PRODUCT_MODIFICATION_DIM_V1 m using(DW_PRODUCTMOD)
		left join LINEITEM_DIM l using(DW_LINEITEM)
	order by DW_GC_HEADER, DW_LINEITEM
	limit 100;

select sum(ACTNETSALES) from TLD_FACT_2007_Q02 left join PRODUCT_DIM using(DW_PRODUCT) where PRODUCTDESC="";

--- check sales volume by item
select DW_PRODUCT, sum(ACTNETSALES), sum(ACTQTYSOLD) from TLD_FACT_2007_Q01 group by DW_PRODUCT into outfile '/gpfs/home/wue04/tb-data/sales-vol-by-product/sales_2007_Q01.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';;
--- check volumes by item, connect with product details
select t.DW_PRODUCTDETAIL, sum(ACTNETSALES), sum(ACTQTYSOLD) from TLD_FACT_2007_Q01 t group by DW_PRODUCTDETAIL into outfile '/gpfs/home/wue04/tb-data/sales-vol-by-product/sales_detail_2007_Q01.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';;

select * from PRODUCT_DETAIL_DIM_V1 into outfile '/gpfs/home/wue04/tb-data/product_detail.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';;



select t.DW_PRODUCTDETAIL, sum(ACTQTYSOLD) from TLD_FACT_2015_Q02 t where DW_RESTID=DW_RESTID=33100|DW_RESTID=154980|DW_RESTID=42499|DW_RESTID=154677|DW_RESTID=149978|DW_RESTID=149988|DW_RESTID=151214|DW_RESTID=39800|DW_RESTID=111265|DW_RESTID=44531|DW_RESTID=152706|DW_RESTID=149979|DW_RESTID=42569|DW_RESTID=154678|DW_RESTID=43393 group by DW_OCCASION, DW_PRODUCTDETAIL
	group by DW_PRODUCTDETAIL into outfile '/gpfs/home/wue04/tb-data/sales-vol-by-product/sales_detail_2007_Q01.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';;
select DW_RESTID, DW_PRODUCTDETAIL, DW_OCCASION, sum(ACTQTYSOLD) from TLD_FACT_2007_Q02 group by DW_OCCASION, DW_RESTID, DW_PRODUCTDETAIL into outfile '/gpfs/home/wue04/tb-data/sales-vol-by-product/sales_byrestid_2007_Q01.csv' fields enclosed by '"' terminated by ';' escaped by '"' lines terminated by '\r\n';;

select DW_RESTID, DW_PRODUCT, ACTPRODPRICE from TLD_FACT_2007_Q02
where DW_DISCOUNT=-1 and DW_PRODUCTMOD=-1 and 
	(DW_PRODUCT=3139 or DW_PRODUCT=3648 or DW_PRODUCT=802 or DW_PRODUCT=4098 or DW_PRODUCT=37956)
group by DW_RESTID, DW_PRODUCT

select count(distinct DW_GC_HEADER) from TLD_FACT_2015_Q01 t
left join PRODUCT_DETAIL_DIM_V1 p using(DW_PRODUCTDETAIL)
	where p.DW_PRODUCTGROUP in (22, 44, 47, 39, 23, 45, 31, 42, 48, 46, 38, 21, 41, 40, 19)
	  or p.PRODUCTDESC regexp 'BYB|AW |AWR|AW,|KFC|LJS|PH |PIZZA HUT|TCBY|ICBIY|KRYSTAL';
select count(distinct DW_GC_HEADER) from TLD_FACT_2015_Q02 t
left join PRODUCT_DETAIL_DIM_V1 p using(DW_PRODUCTDETAIL)
	where p.DW_PRODUCTGROUP in (22, 44, 47, 39, 23, 45, 31, 42, 48, 46, 38, 21, 41, 40, 19)
	  or p.PRODUCTDESC regexp 'BYB|AW |AWR|AW,|KFC|LJS|PH |PIZZA HUT|TCBY|ICBIY|KRYSTAL';
select count(distinct DW_GC_HEADER) from TLD_FACT_2015_Q03 t
left join PRODUCT_DETAIL_DIM_V1 p using(DW_PRODUCTDETAIL)
	where p.DW_PRODUCTGROUP in (22, 44, 47, 39, 23, 45, 31, 42, 48, 46, 38, 21, 41, 40, 19)
	  or p.PRODUCTDESC regexp 'BYB|AW |AWR|AW,|KFC|LJS|PH |PIZZA HUT|TCBY|ICBIY|KRYSTAL';

select DW_GC_HEADER, DW_PRODUCT, p.PRODUCTDESC, DW_PRODUCTDETAIL, d.PRODUCTDESC, DW_PRODUCTMOD, m.PRODUCTDESC, DW_LINEITEM, l.LINEITEMDESC from TLD_FACT_2012_Q01 t
	left join PRODUCT_DIM p using(DW_PRODUCT)
	left join PRODUCT_DETAIL_DIM_V1 d using(DW_PRODUCTDETAIL)
	left join LINEITEM_DIM l using(DW_LINEITEM)
	left join PRODUCT_MODIFICATION_DIM_V1 m using(DW_PRODUCTMOD)
	where DW_GC_HEADER=4253668892
	order by DW_LINEITEM;
	