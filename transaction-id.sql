/* -- access tacobell database
srun  --time=1-00:01:00 --pty --mem-per-cpu=8G bash 
module load mariadb/10.4.6
mysql -p -h db -P 33061 -u wue04 tacobell --plugin-dir=/gpfs/share/apps/mariadb/10.4.6/lib/plugin

srun  --time=1-00:01:00 --pty --mem-per-cpu=8G bash
module load mariadb/5.5.64
mysql -p -h db -P 33061 -u wue04 tacobell


-- for new database tacobell15
srun  --time=1-00:01:00 --pty --mem-per-cpu=8G bash 
module load mariadb/10.4.6
mysql -p -h db -P 33062 -u wue04 -S /gpfs/data/tb_project/tacobell15/mysql.sock
SET PASSWORD FOR 'wue04'@'172.16.0.%' = PASSWORD('newpass');

-- access tacobell2 on the existing tacobell socket
srun  --time=1-00:01:00 --pty --mem-per-cpu=8G bash 
module load mariadb/5.5.64
mysql -p -h db -P 33061 -u wue04 tacobell2
--use tacobell2;

--- run jobs on hpc
sbatch tacobell/ssb-sales-2015-q1.sh
ll -rt
squeue -u wue04

-- run SQL squirrel on HPC
-- launch Xming first
srun --partition=cpu_short --ntasks=2 --cpus-per-task=1 --mem-per-cpu=8G --time=23:00:00 --x11 --pty bash
module load mariadb/5.5.64 squirrel/4.0.0
java -jar $SQUIRREL_ROOT/squirrel-sql.jar
*/

select (ItemCount.sales / TotalCount.total *100) as ItemRatio
from 
  (select count(distinct DW_GC_HEADER) as sales from TLD_FACT_2014_Q01 where DW_PRODUCT=30839) as ItemCount,
  (select count(distinct DW_GC_HEADER) as total from TLD_FACT_2014_Q01) as TotalCount;
select (ItemCount.sales / TotalCount.total *100) as ItemRatio
from 
  (select count(distinct DW_GC_HEADER) as sales from TLD_FACT_2014_Q02 where DW_PRODUCT=30839) as ItemCount,
  (select count(distinct DW_GC_HEADER) as total from TLD_FACT_2014_Q02) as TotalCount;
select (ItemCount.sales / TotalCount.total *100) as ItemRatio
from 
  (select count(distinct DW_GC_HEADER) as sales from TLD_FACT_2014_Q03 where DW_PRODUCT=30839) as ItemCount,
  (select count(distinct DW_GC_HEADER) as total from TLD_FACT_2014_Q03) as TotalCount;
select (ItemCount.sales / TotalCount.total *100) as ItemRatio
from 
  (select count(distinct DW_GC_HEADER) as sales from TLD_FACT_2014_Q04 where DW_PRODUCT=30839) as ItemCount,
  (select count(distinct DW_GC_HEADER) as total from TLD_FACT_2014_Q04) as TotalCount;



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

--- create new table for calorie info
--- make sure every cell value is enclosed by "", incl numeric values
create table calorietest (
	DW_PRODUCT int not null,
	DW_PRODUCTGROUP int not null,
	PRODUCTDESC varchar(255) not null,
	FULLDESC varchar(255) null,
	CALORIES decimal(10, 2) null,
	TOTAL_FAT decimal(10, 2) null,
	SAT_FAT decimal(10, 2) null,
	TRANS_FAT decimal(10, 2) null,	
	CHOLESTEROL decimal(10, 2) null,
	SODIUM decimal(10, 2) null,
	POTASSIUM decimal(10, 2) null,
	CARB decimal(10, 2) null,
	FIBER decimal(10, 2) null,
	SUGAR decimal(10, 2) null,
	PROTEIN decimal(10, 2) null,
	primary key (DW_PRODUCT)
);

load data infile '/gpfs/home/wue04/tb-data/menu-matching-to-bigpurple/PRODUCT_CALORIE_DIM.csv'
into table calorietest
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS
(DW_PRODUCT,DW_PRODUCTGROUP,PRODUCTDESC,@vFULLDESC,@vCALORIES,@vTOTAL_FAT,@vSAT_FAT,@vTRANS_FAT,@vCHOLESTEROL,@vSODIUM,@vPOTASSIUM,@vCARB,@vFIBER,@vSUGAR,@vPROTEIN) 
set
FULLDESC=NULLIF(@vFULLDESC, ''),
CALORIES=NULLIF(@vCALORIES, ''),
TOTAL_FAT=NULLIF(@vTOTAL_FAT, ''),
SAT_FAT=NULLIF(@vSAT_FAT, ''),
TRANS_FAT=NULLIF(@vTRANS_FAT, ''),
CHOLESTEROL=NULLIF(@vCHOLESTEROL, ''),
SODIUM=NULLIF(@vSODIUM, ''),
POTASSIUM=NULLIF(@vPOTASSIUM, ''),
CARB=NULLIF(@vCARB, ''),
FIBER=NULLIF(@vFIBER, ''),
SUGAR=NULLIF(@vSUGAR, ''),
PROTEIN=NULLIF(@vPROTEIN, '')
;
--- test
select * from calorietest limit 10;

select sum(c.CALORIES), avg(c.CALORIES), avg(c.TOTAL_FAT) from TLD_FACT_2007_Q01 t
left join calorietest c using(DW_PRODUCT)
group by DW_OCCASION 
order by  DW_OCCASION;

--- mean calorie per order
select t.DW_RESTID, y.DW_YEAR, y.DW_MONTH, sum(c.CALORIES*t.ACTQTYSOLD)/count(distinct t.DW_GC_HEADER) from TLD_FACT_2007_Q02 t
left join calorietest c using(DW_PRODUCT)
left join TIME_DAY_DIM y using(DW_DAY)
group by DW_RESTID, DW_YEAR, DW_MONTH
limit 10;

---- add total calories of each order to GC_HEADER_DIM_2007_Q01 table
alter table GC_HEADER_DIM_2007_Q01
add column CALORIES decimal(10,2) after DW_CHECK_IN_LOCATION_IDENTIFIER;

insert into GC_HEADER_DIM_2007_Q01(CALORIES) 
select sum(n.CALORIES*t.ACTQTYSOLD)/2 from TLD_FACT_2007_Q01 t
left join nutrition n using(DW_PRODUCT) 
group by DW_GC_HEADER;

--- create smaller test table
create table GC_HEADER_CALORIE_2007_Q04 (
	DW_GC_HEADER bigint(20) not null primary key,
	CALORIES decimal(10,2)
);
insert into GC_HEADER_CALORIE_2007_Q04(DW_GC_HEADER, CALORIES) 
select DW_GC_HEADER, sum(n.CALORIES*t.ACTQTYSOLD) as CALORIES from TLD_FACT_2007_Q04 t
left join nutrition n on n.DW_PRODUCT=t.DW_PRODUCTDETAIL 
group by DW_GC_HEADER;
----select * from GC_HEADER_CALORIE_2007_Q04 limit 20;


+--------------+--------------------------------+
| DW_GC_HEADER | sum(n.CALORIES*t.ACTQTYSOLD)/2 |
+--------------+--------------------------------+
|   2193837493 |                  1610.45000000 |
|   2193837494 |                   781.11000000 |
|   2193837495 |                  1038.00000000 |
|   2193837496 |                  1700.00000000 |
|   2193837497 |                   196.00000000 |
|   2193837498 |                   540.00000000 |
|   2193837499 |                   772.22000000 |
|   2193837500 |                  1053.34000000 |
|   2193837501 |                   764.44000000 |
|   2193837502 |                  1011.67000000 |
|   2193837503 |                   690.00000000 |
|   2193837504 |                   552.50000000 |
|   2193837505 |                   302.50000000 |
|   2193837506 |                   572.00000000 |
|   2193837507 |                  1153.35000000 | 
|   2193837508 |                   742.84000000 |
|   2193837509 |                   537.78000000 |
|   2193837510 |                   392.00000000 |
|   2193837511 |                           NULL |
|   2193837512 |                   925.84000000 |
+--------------+--------------------------------+
SELECT t.DW_GC_HEADER, t.DW_PRODUCT, p.PRODUCTDESC, t.DW_PRODUCTDETAIL, d.PRODUCTDESC, t.ACTQTYSOLD, n.CALORIES
from TLD_FACT_2007_Q02 t
left join PRODUCT_DIM p using(DW_PRODUCT)
left join PRODUCT_DETAIL_DIM_V1 d USING(DW_PRODUCTDETAIL)
left join nutrition n on n.DW_PRODUCT=t.DW_PRODUCTDETAIL
where DW_GC_HEADER=2267915563;


create table GC_HEADER_CALORIE_2015_Q01 (
	DW_GC_HEADER bigint(20) not null primary key,
	CALORIES decimal(10,2)
);
insert into GC_HEADER_CALORIE_2015_Q01(DW_GC_HEADER, CALORIES) 
select DW_GC_HEADER, sum(n.CALORIES*t.ACTQTYSOLD) as CALORIES from TLD_FACT_2015_Q01 t
left join nutrition n on n.DW_PRODUCT=t.DW_PRODUCTDETAIL 
group by DW_GC_HEADER;



--- add order modifications to calorie count
select y.DW_YEAR, y.DW_MONTH, 
    sum(n1.CALORIES*t.ACTQTYSOLD*l.ITEMMOD + n2.CALORIES*t.ACTMODQTY*l.ITEMMOD)/count(distinct t.DW_GC_HEADER) as mean_cal,
    sum(n1.SAT_FAT*t.ACTQTYSOLD*l.ITEMMOD + n2.SAT_FAT*t.ACTMODQTY*l.ITEMMOD)/count(distinct t.DW_GC_HEADER) as mean_satfat,
    sum(n1.CARB*t.ACTQTYSOLD*l.ITEMMOD + n2.CARB*t.ACTMODQTY*l.ITEMMOD)/count(distinct t.DW_GC_HEADER) as mean_carb,
    sum(n1.PROTEIN*t.ACTQTYSOLD*l.ITEMMOD + n2.PROTEIN*t.ACTMODQTY*l.ITEMMOD)/count(distinct t.DW_GC_HEADER) as mean_protein,
    sum(n1.SODIUM*t.ACTQTYSOLD*l.ITEMMOD + n2.SODIUM*t.ACTMODQTY*l.ITEMMOD)/count(distinct t.DW_GC_HEADER) as mean_sodium
    from TLD_FACT_2007_Q01 t
	left join LINEITEM_DIM l using(DW_LINEITEM)
	left join TIME_DAY_DIM y using(DW_DAY)
    left join nutrition n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
    left join nutrition n2 on n2.DW_PRODUCT=t.DW_PRODUCTMOD
    group by DW_YEAR, DW_MONTH

insert into LINEITEM_DIM(DW_LINEITEM, ITEMMOD) values (-1,0),(1,1),(2,1),(3,1),(4,1),(5,-1),(6,1),(7,1),(8,-1),(9,0.5),(10,1),(11,-1),(12,0.5),(13,1),(14,0),(15,1),(16,1)
on duplicate key update DW_LINEITEM=VALUES(DW_LINEITEM), ITEMMOD=VALUES(ITEMMOD);
select * from LINEITEM_DIM order by DW_LINEITEM;


select t.DW_GC_HEADER, t.DW_LINEITEM, l.LINEITEMDESC, l.ITEMMOD,
	t.ACTPRODPRICE, t.ACTQTYSOLD, t.ACTMODQTY,
	t.DW_PRODUCT, p.PRODUCTDESC as product,
	t.DW_PRODUCTDETAIL, d.PRODUCTDESC as detail, n1.CALORIES,
	t.DW_PRODUCTMOD, m.PRODUCTDESC as modif, n2.CALORIES,
	sum(COALESCE(n1.CALORIES,0)*t.ACTQTYSOLD*l.ITEMMOD + COALESCE(n2.CALORIES,0)*t.ACTMODQTY*l.ITEMMOD) as cal
from TLD_FACT_2007_Q02 t
left join PRODUCT_DIM p using(DW_PRODUCT)
left join PRODUCT_DETAIL_DIM_V1 d using(DW_PRODUCTDETAIL)
left join PRODUCT_MODIFICATION_DIM_V1 m using(DW_PRODUCTMOD)
left join LINEITEM_DIM l using(DW_LINEITEM)
left join nutrition n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
left join nutrition n2 on n2.DW_PRODUCT=t.DW_PRODUCTMOD
where DW_GC_HEADER=2267915567
order by DW_GC_HEADER
limit 25;
+--------------+-------------+-------------------------+---------+--------------+------------+-----------+------------+----------------------------+------------------+----------------------------+----------+---------------+------------------------+----------+
| DW_GC_HEADER | DW_LINEITEM | LINEITEMDESC            | ITEMMOD | ACTPRODPRICE | ACTQTYSOLD | ACTMODQTY | DW_PRODUCT | product                    | DW_PRODUCTDETAIL | detail                     | CALORIES | DW_PRODUCTMOD | modif                  | CALORIES |
+--------------+-------------+-------------------------+---------+--------------+------------+-----------+------------+----------------------------+------------------+----------------------------+----------+---------------+------------------------+----------+
|   2267915567 |          14 | TAX-LINE                |     0.0 |         0.00 |       0.00 |       0.0 |         -1 | N/A                        |               -1 | N/A                        |     NULL |            -1 | N/A                    |     NULL |
|   2267915567 |           1 | COMBO-ITEM              |     1.0 |         2.60 |       1.00 |       0.0 |       1692 | COMBO 7                    |             1692 | COMBO 7                    |     NULL |            -1 | N/A                    |     NULL |
|   2267915567 |           7 | COMBO-MOD-INGRD-ADD     |     1.0 |         0.00 |       0.00 |       1.0 |       1692 | COMBO 7                    |             4450 | CRUNCHY TACO BEEF          |   170.00 |          4160 | SUB SOFT FOR HARD TACO |     NULL |
|   2267915567 |           3 | COMBO-DETAIL            |     1.0 |         0.00 |       1.00 |       0.0 |       1692 | COMBO 7                    |             3844 | QUESADILLA CHICKEN         |   515.56 |            -1 | N/A                    |     NULL |
|   2267915567 |           3 | COMBO-DETAIL            |     1.0 |         0.00 |       1.00 |       0.0 |       1692 | COMBO 7                    |             4450 | CRUNCHY TACO BEEF          |   170.00 |            -1 | N/A                    |     NULL |
|   2267915567 |           2 | NON-COMBO-ITEM          |     1.0 |         1.49 |       1.00 |       0.0 |       3649 | LARGE PEPSI                |             3649 | LARGE PEPSI                |   375.00 |            -1 | N/A                    |     NULL |
+--------------+-------------+-------------------------+---------+--------------+------------+-----------+------------+----------------------------+------------------+----------------------------+----------+---------------+------------------------+----------+

select t.DW_RESTID, a.ADDRESS_LINE_1, a.STATENAME, a.COUNTYNAME, t.DW_PRODUCT from TLD_FACT_2008_Q02 t
left join ALIGN_DIM a using(DW_RESTID)
where STATENAME="NY" and COUNTYNAME="QUEENS"
order by ADDRESS_LINE_1
LIMIT 10;


select t.DW_RESTID, t.DW_PRODUCT, p.PRODUCTDESC from TLD_FACT_2008_Q02 t
left join PRODUCT_DIM p using(DW_PRODUCT)
where DW_RESTID=35251
order by DW_GC_HEADER
limit 20;



select t.DW_GC_HEADER,  sum(*t.ACTQTYSOLD) as ssb from TLD_FACT_2015_Q01 t
	left join nutrition n on n.DW_PRODUCT=t.DW_PRODUCTDETAIL
	left join TIME_DAY_DIM d using(DW_DAY)
	left join TIME_MINUTE_DIM m using(DW_MINUTE)
where n.DW_PRODUCTGROUP=16 and d.DW_FISCALWEEK=1308
group by DW_GC_HEADER
order by DW_RESTID, DW_MINUTE;

select t.DW_GC_HEADER as transaction_id, d.BUSIDAYNAME, d.BUSIDAYDT, m.MINUTETM,
	t.DW_RESTID as restaurant_id, t.DW_PRODUCTDETAIL as beverage_id, det.PRODUCTDESC as beverage_name,
	n.CALORIES as cal, t.ACTQTYSOLD as quantity
FROM TLD_FACT_2015_Q01 t
	left join PRODUCT_DETAIL_DIM_V1 det using(DW_PRODUCTDETAIL)
	left join nutrition n on n.DW_PRODUCT=t.DW_PRODUCTDETAIL
	left join TIME_DAY_DIM d using(DW_DAY)
	left join TIME_MINUTE_DIM m using(DW_MINUTE)
where det.DW_PRODUCTGROUP=16 and d.DW_FISCALWEEK=1308 and t.ACTQTYSOLD>0
order by DW_RESTID, DW_DAY, DW_MINUTE
limit 20;

+--------------+-----------------+------------+------------------------+------------------+-------------------------+------------+
| DW_GC_HEADER | DW_PRODUCTGROUP | DW_PRODUCT | prod                   | DW_PRODUCTDETAIL | detail                  | ACTQTYSOLD |
+--------------+-----------------+------------+------------------------+------------------+-------------------------+------------+
|   6823766137 |              -1 |         -1 | N/A                    |               -1 | N/A                     |       0.00 |
|   6823766138 |              29 |       1680 | COMBO 1                |             1680 | COMBO 1                 |       1.00 |
|   6823766138 |              11 |       3361 | NACHOS                 |             3361 | NACHOS                  |       1.00 |
|   6823766138 |              29 |       1680 | COMBO 1                |             4787 | LARGE BEVERAGE          |       1.00 |
|   6823766138 |              29 |       1680 | COMBO 1                |             4457 | CRUNCHY TACO SUPREME BF |       1.00 |
|   6823766138 |              29 |       1680 | COMBO 1                |              635 | BURRITO SUPREME BEEF    |       1.00 |
|   6823766139 |              29 |       1680 | COMBO 1                |             1680 | COMBO 1                 |       1.00 |
|   6823766139 |              29 |       1680 | COMBO 1                |              635 | BURRITO SUPREME BEEF    |       1.00 |
|   6823766139 |              29 |       1680 | COMBO 1                |             4787 | LARGE BEVERAGE          |       1.00 |
|   6823766139 |              29 |       1680 | COMBO 1                |             4457 | CRUNCHY TACO SUPREME BF |       1.00 |
+--------------+-----------------+------------+------------------------+------------------+-------------------------+------------+

select DW_RESTID, ADDRESS_LINE_1, CITYNAME, STATENAME, OPENEDDT, TEMPCLOSEDDT, REOPENDT, CLOSEDDT
from ALIGN_DIM
where ADDRESS_LINE_1="3000 ISLAND AVE";

select count(distinct DW_GC_HEADER) from TLD_FACT_2007_Q01
where DW_RESTID=35251;

select count(distinct t.DW_GC_HEADER), d.BUSIDAYDT from TLD_FACT_2009_Q03 t
	left join TIME_DAY_DIM d using(DW_DAY)
where DW_RESTID=35251
group by d.BUSIDAYDT
order by d.BUSIDAYDT
limit 10;

select count(distinct t.DW_GC_HEADER), d.BUSIDAYDT from TLD_FACT_2011_Q01 t
	left join TIME_DAY_DIM d using(DW_DAY)
	left join ALIGN_DIM a using(DW_RESTID)
where a.STATENAME="ME"
group by d.BUSIDAYDT
order by d.BUSIDAYDT
limit 100;

select DW_RESTID, ADDRESS_LINE_1, CITYNAME, STATENAME, OPENEDDT, TEMPCLOSEDDT, REOPENDT, CLOSEDDT
FROM ALIGN_DIM
WHERE STATENAME="ME" and OPENEDDT<="2010-12-31" and (CLOSEDDT="0000-00-00" or CLOSEDDT>="2010-12-31")
order by OPENEDDT;



--- create new table for calorie info
--- make sure every cell value is enclosed by "", incl numeric values
create table nutrition (
	DW_PRODUCT int not null,
	DW_PRODUCTGROUP int not null,
	PRODUCTDESC varchar(255) not null,
	FULLDESC varchar(255) null,
	CALORIES decimal(10, 2) null,
	TOTAL_FAT decimal(10, 2) null,
	SAT_FAT decimal(10, 2) null,
	TRANS_FAT decimal(10, 2) null,	
	CHOLESTEROL decimal(10, 2) null,
	SODIUM decimal(10, 2) null,
	POTASSIUM decimal(10, 2) null,
	CARB decimal(10, 2) null,
	FIBER decimal(10, 2) null,
	SUGAR decimal(10, 2) null,
	PROTEIN decimal(10, 2) null,
	primary key (DW_PRODUCT)
);

load data infile '/gpfs/home/wue04/tb-data/menu-matching-to-bigpurple/PRODUCT_CALORIE_DIM-drinks-fixed.csv'
into table nutrition
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS
(DW_PRODUCT,DW_PRODUCTGROUP,PRODUCTDESC,@vFULLDESC,@vCALORIES,@vTOTAL_FAT,@vSAT_FAT,@vTRANS_FAT,@vCHOLESTEROL,@vSODIUM,@vPOTASSIUM,@vCARB,@vFIBER,@vSUGAR,@vPROTEIN) 
set
FULLDESC=NULLIF(@vFULLDESC, ''),
CALORIES=NULLIF(@vCALORIES, ''),
TOTAL_FAT=NULLIF(@vTOTAL_FAT, ''),
SAT_FAT=NULLIF(@vSAT_FAT, ''),
TRANS_FAT=NULLIF(@vTRANS_FAT, ''),
CHOLESTEROL=NULLIF(@vCHOLESTEROL, ''),
SODIUM=NULLIF(@vSODIUM, ''),
POTASSIUM=NULLIF(@vPOTASSIUM, ''),
CARB=NULLIF(@vCARB, ''),
FIBER=NULLIF(@vFIBER, ''),
SUGAR=NULLIF(@vSUGAR, ''),
PROTEIN=NULLIF(@vPROTEIN, '')
;


---- product category; for by food group analysis
create table product_category (
	DW_PRODUCT int not null,
	CATEGORY varchar(255) not null,
	DW_PRODUCTGROUP int not null,
	PRODUCT varchar(255) not null,
	DW_CATEGORY int not null,
	primary key (DW_PRODUCT)
);
load data infile '/gpfs/home/wue04/tb-data/product-category-to-bigpurple/product-category.csv'
into table product_category
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

select y.DW_YEAR, y.DW_MONTH, t.DW_RESTID, c.DW_CATEGORY, t.DW_OCCASION,
    sum(coalesce(n1.CALORIES,0)*t.ACTQTYSOLD*l.ITEMMOD + coalesce(n2.CALORIES,0)*t.ACTMODQTY*l.ITEMMOD) as cal,
	count(distinct t.DW_GC_HEADER) as count
    from TLD_FACT_2007_Q02 t
	left join LINEITEM_DIM l using(DW_LINEITEM)
	left join TIME_DAY_DIM y using(DW_DAY)
    left join nutrition n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
    left join nutrition n2 on n2.DW_PRODUCT=t.DW_PRODUCTMOD
	left join product_category c on t.DW_PRODUCTDETAIL=c.DW_PRODUCT
    group by DW_YEAR, DW_MONTH, DW_RESTID, DW_CATEGORY, DW_OCCASION
	

select t.DW_GC_HEADER, l.LINEITEMDESC, l.ITEMMOD,
        t.DW_PRODUCT, p.PRODUCTDESC, 
        t.DW_PRODUCTDETAIL, d.PRODUCTDESC, t.ACTQTYSOLD, n1.CALORIES,
        t.DW_PRODUCTMOD, m.PRODUCTDESC, n2.CALORIES, t.ACTMODQTY
    from TLD_FACT_2007_Q02 t
	left join LINEITEM_DIM l on l.DW_LINEITEM=t.DW_LINEITEM
	left join PRODUCT_DETAIL_DIM_V1 d on t.DW_PRODUCTDETAIL=d.DW_PRODUCTDETAIL
	left join PRODUCT_DIM p on t.DW_PRODUCT=p.DW_PRODUCT
	left join PRODUCT_MODIFICATION_DIM_V1 m on t.DW_PRODUCTMOD=m.DW_PRODUCTMOD
	left join nutrition n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
    left join nutrition n2 on n2.DW_PRODUCT=t.DW_PRODUCTMOD
	where DW_GC_HEADER=2267915567
	order by DW_GC_HEADER, t.DW_LINEITEM;
+--------------+---------------------+---------+------------+-------------+------------+------------------+--------------------+---------------+------------------------+-----------+
| DW_GC_HEADER | LINEITEMDESC        | ITEMMOD | DW_PRODUCT | PRODUCTDESC | ACTQTYSOLD | DW_PRODUCTDETAIL | PRODUCTDESC        | DW_PRODUCTMOD | PRODUCTDESC            | ACTMODQTY |
+--------------+---------------------+---------+------------+-------------+------------+------------------+--------------------+---------------+------------------------+-----------+
|   2267915567 | COMBO-ITEM          |     1.0 |       1692 | COMBO 7     |       1.00 |             1692 | COMBO 7            |            -1 | N/A                    |       0.0 |
|   2267915567 | NON-COMBO-ITEM      |     1.0 |       3649 | LARGE PEPSI |       1.00 |             3649 | LARGE PEPSI        |            -1 | N/A                    |       0.0 |
|   2267915567 | COMBO-DETAIL        |     1.0 |       1692 | COMBO 7     |       1.00 |             3844 | QUESADILLA CHICKEN |            -1 | N/A                    |       0.0 |
|   2267915567 | COMBO-DETAIL        |     1.0 |       1692 | COMBO 7     |       1.00 |             4450 | CRUNCHY TACO BEEF  |            -1 | N/A                    |       0.0 |
|   2267915567 | COMBO-MOD-INGRD-ADD |     1.0 |       1692 | COMBO 7     |       0.00 |             4450 | CRUNCHY TACO BEEF  |          4160 | SUB SOFT FOR HARD TACO |       1.0 |
|   2267915567 | TAX-LINE            |     0.0 |         -1 | N/A         |       0.00 |               -1 | N/A                |            -1 | N/A                    |       0.0 |
+--------------+---------------------+---------+------------+-------------+------------+------------------+--------------------+---------------+------------------------+-----------+

create table restaurant_in_use_match_drive_thru (
	address varchar(255) not null,
	treat int not null,
	DW_RESTID int not null,
	primary key (DW_RESTID)
);
load data infile '/gpfs/home/wue04/tb-data/upload-to-bigpurple/restaurants-in-use-matching-drive-thru.csv'
into table restaurant_in_use_match_drive_thru
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;


select t.DW_GC_HEADER, t.DW_PRODUCT, p.PRODUCTDESC, t.DW_PRODUCTDETAIL, d.PRODUCTDESC, t.ACTQTYSOLD
    from TLD_FACT_2007_Q02 t
	left join PRODUCT_DETAIL_DIM_V1 d on t.DW_PRODUCTDETAIL=d.DW_PRODUCTDETAIL
	left join PRODUCT_DIM p on t.DW_PRODUCT=p.DW_PRODUCT
	where t.DW_PRODUCT=1688
	order by DW_GC_HEADER, DW_PRODUCT
	limit 50;
select y.DW_YEAR, y.DW_MONTH,  c.DW_CATEGORY, t.DW_OCCASION,
    sum(t.ACTQTYSOLD) as sales
    from TLD_FACT_2007_Q02 t
    left join TIME_DAY_DIM y using(DW_DAY)
    left join product_category c on t.DW_PRODUCTDETAIL=c.DW_PRODUCT
    group by DW_YEAR, DW_MONTH, DW_CATEGORY, DW_OCCASION;


create table consistent_for_sale_product_2010_onward (
	DW_PRODUCT int not null,
	primary key (DW_PRODUCT)
);
load data infile '/gpfs/home/wue04/tb-data/upload-to-bigpurple/consistent-sales-2010onward-forCA.csv'
into table consistent_for_sale_product_2010_onward
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;


select y.DW_YEAR, y.DW_MONTH, t.DW_RESTID, c.DW_CATEGORY,
    sum(coalesce(n1.CALORIES,0)*t.ACTQTYSOLD*l.ITEMMOD + coalesce(n2.CALORIES,0)*t.ACTMODQTY*l.ITEMMOD) as cal,
	count(distinct t.DW_GC_HEADER) as count
    from TLD_FACT_2007_Q02 t
	inner join consistent_for_sale_product_2010_onward s on s.DW_PRODUCT=t.DW_PRODUCTDETAIL
	left join LINEITEM_DIM l using(DW_LINEITEM)
	left join TIME_DAY_DIM y using(DW_DAY)
    left join nutrition n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
    left join nutrition n2 on n2.DW_PRODUCT=t.DW_PRODUCTMOD
	left join product_category c on t.DW_PRODUCTDETAIL=c.DW_PRODUCT
	where DW_OCCASION=2
    group by DW_YEAR, DW_MONTH, DW_RESTID, DW_CATEGORY;

select y.DW_YEAR, y.DW_MONTH, c.DW_CATEGORY, count(distinct t.DW_GC_HEADER) as total_num_orders, sum(t.ACTQTYSOLD) as qty_sold
from TLD_FACT_2007_Q02 t
left join TIME_DAY_DIM y using(DW_DAY)
left join product_category c on t.DW_PRODUCTDETAIL=c.DW_PRODUCT
group by DW_YEAR, DW_MONTH, DW_CATEGORY;







create table test_occasion (
	dw_occasion int not null,
	occasioncd varchar(3),
	occasiondesc varchar(30),
	primary key (dw_occasion)
);
load data local infile '/gpfs/data/elbellab/tb-bi-prod-user-data/OCCASION_DIM.csv'
into table test_occasion
FIELDS TERMINATED BY '|' 
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

-- 1 line solution
cat GC_HEADER_DIM_2016_Q1_*.gz | gunzip -c > GC_HEADER_DIM_2016_Q1.csv
-- 2 line solution
gunzip GC_HEADER_DIM_2015_Q4_*
cat GC_HEADER_DIM_2015_Q4_1.csv GC_HEADER_DIM_2015_Q4_2.csv GC_HEADER_DIM_2015_Q4_3.csv > GC_HEADER_DIM_2015_Q4.csv

create table GC_HEADER_DIM_2015_Q4 (
	dw_gc_header bigint(20) not null,
	dw_day int(11) NOT NULL,
	dw_minute smallint(6) NOT NULL,
	dw_restid int(11) NOT NULL,
	dw_occasion smallint(6) NOT NULL,
	dw_tendertype smallint(6) NOT NULL,
	ticketno int(11) NOT NULL,
	firstitemtime timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	storedordertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
	recalledordertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
	amounttendertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
	servedordertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
	ordertotaledtime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
	parkedordertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
	registerno smallint(6) NOT NULL,
	registerseqno smallint(6) DEFAULT NULL,
	totpromosales decimal(6,2) NOT NULL,
	totgrosssales decimal(6,2) NOT NULL,
	totnetsales decimal(6,2) NOT NULL,
	totdiscountsales decimal(6,2) NOT NULL,
	totdiscpct decimal(3,2) NOT NULL,
	tottax decimal(6,2) NOT NULL,
	refundflag char(1) NOT NULL,
	cashierid int(11) DEFAULT NULL,
	dw_channel smallint(6) NOT NULL,
	ordercheckintime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
	dw_consumer_identifier bigint(20) DEFAULT NULL,
	dw_check_in_location_identifier smallint(6) NOT NULL,
	order_source int default null,
	xenial_order_id int default null,
	dw_tax_flag int default null,
	primary key (dw_gc_header)
);
load data local infile '/gpfs/data/elbellab/tb-bi-prod-user-data/GC_HEADER_DIM_2015_Q4.csv'
into table GC_HEADER_DIM_2015_Q4
FIELDS TERMINATED BY '|' 
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;













select *
from (select *, rank() over (partition by full_table.DW_RESTID, full_table.DW_MONTH order by full_table.qty desc) as ranking
from (select t.DW_RESTID, y.DW_MONTH, sum(t.ACTQTYSOLD) as qty, t.DW_PRODUCTDETAIL
    from TLD_FACT_2007_Q02 t
	left join TIME_DAY_DIM y using(DW_DAY)
	inner join restaurant_in_use_match_drive_thru r on r.DW_RESTID=t.DW_RESTID
	left join nutrition n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
	where n1.calorie is not NULL
	group by DW_RESTID, DW_MONTH, DW_PRODUCTDETAIL
) as full_table
) as ranked_table
where ranking<=20;


select t.DW_RESTID, y.DW_MONTH, sum(t.ACTQTYSOLD) as qty, t.DW_PRODUCTDETAIL
    from TLD_FACT_2007_Q02 t
	left join TIME_DAY_DIM y using(DW_DAY)
	inner join restaurant_in_use_match_drive_thru r on r.DW_RESTID=t.DW_RESTID
	left join nutrition n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
	where n1.CALORIES is not NULL
	group by DW_RESTID, DW_MONTH, DW_PRODUCTDETAIL


select distinct(t.DW_PRODUCTDETAIL) as product
from TLD_FACT_2007_Q01 t
inner join restaurant_in_use_match_drive_thru r on r.DW_RESTID=t.DW_RESTID
left join nutrition n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
where n1.CALORIES is not NULL
group by DW_PRODUCTDETAIL;

select count(DW_GC_HEADER) from GC_HEADER_DIM_2012_Q03;
select count(DISTINCT DW_GC_HEADER) from TLD_FACT_2012_Q03;
select count(distinct DW_GC_HEADER)
    from TLD_FACT_2012_Q03 t
	left join TIME_DAY_DIM y using(DW_DAY)
	where DW_MONTH=272;


select y.DW_MONTH, r.DW_RESTID, t.DW_PRODUCTDETAIL, n1.CALORIES, n1.FULLDESC,
    sum(t.ACTQTYSOLD) as qty
    from TLD_FACT_2007_Q02 t
	inner join restaurant_in_use_match_drive_thru r on r.DW_RESTID=t.DW_RESTID
	left join TIME_DAY_DIM y using(DW_DAY)
    left join nutrition n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
	where DW_OCCASION=2 and CALORIES is not NULL
    group by DW_MONTH, DW_RESTID, DW_PRODUCTDETAIL;


claim_uhc = dense_rank() over (partition by PERSON_ID,ServiceStartDateYear,ServiceStartMonth order by (case when BillingTaxId='135563408' then ClaimNr else '0' end)) + 
					dense_rank() over (partition by PERSON_ID,ServiceStartDateYear,ServiceStartMonth order by (case when BillingTaxId='135563408' then ClaimNr else '0' end) desc) -
					case when count(*) over(partition by PERSON_ID,ServiceStartDateYear,ServiceStartMonth)=sum(case when BillingTaxId='135563408' then 1 else 0 end) over(partition by PERSON_ID,ServiceStartDateYear,ServiceStartMonth)
					then 1 else 2 end,

create table GC_HEADER_DIM_2015_Q4 (
dw_gc_header bigint(20) not null,
dw_day int(11) NOT NULL,
dw_minute smallint(6) NOT NULL,
dw_restid int(11) NOT NULL,
dw_occasion smallint(6) NOT NULL,
dw_tendertype smallint(6) NOT NULL,
ticketno int(11) NOT NULL,
firstitemtime timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
storedordertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
recalledordertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
amounttendertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
servedordertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
ordertotaledtime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
parkedordertime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
registerno smallint(6) NOT NULL,
registerseqno smallint(6) DEFAULT NULL,
totpromosales decimal(6,2) NOT NULL,
totgrosssales decimal(6,2) NOT NULL,
totnetsales decimal(6,2) NOT NULL,
totdiscountsales decimal(6,2) NOT NULL,
totdiscpct decimal(3,2) NOT NULL,
tottax decimal(6,2) NOT NULL,
refundflag char(1) NOT NULL,
cashierid int(11) DEFAULT NULL,
dw_channel smallint(6) NOT NULL,
ordercheckintime timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
dw_consumer_identifier bigint(20) DEFAULT NULL,
dw_check_in_location_identifier smallint(6) NOT NULL,
order_source int default null,
xenial_order_id int default null,
dw_tax_flag int default null,
primary key (dw_gc_header)
);
load data local infile '/gpfs/data/elbellab/tb-bi-prod-user-data/GC_HEADER_DIM_2015_Q4.csv'
into table GC_HEADER_DIM_2015_Q4
FIELDS TERMINATED BY '|' 
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

select t.DW_RESTID, y.DW_MONTH, t.DW_OCCASION, count(distinct DW_GC_HEADER),
sum(coalesce(n1.CALORIES,0)*t.ACTQTYSOLD*l.ITEMMOD + coalesce(n2.CALORIES,0)*t.ACTMODQTY*l.ITEMMOD) as cal
from TLD_FACT_2007_Q01 t
left join LINEITEM_DIM l using(DW_LINEITEM)
left join TIME_DAY_DIM y using(DW_DAY)
left join nutrition_view n1 on n1.DW_PRODUCT=t.DW_PRODUCTDETAIL
left join nutrition_view n2 on n2.DW_PRODUCT=t.DW_PRODUCTMOD
left join PRODUCT_DETAIL_DIM_V1 g on g.DW_PRODUCTDETAIL=t.DW_PRODUCTDETAIL
where g.DW_PRODUCTGROUP between 15 and 17
group by DW_RESTID, DW_MONTH, DW_OCCASION