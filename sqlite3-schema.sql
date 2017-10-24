CREATE TABLE `alerts` (
  `symbol` varchar(12) DEFAULT NULL
,  `timeframe` varchar(3) DEFAULT NULL
,  `datetime` datetime DEFAULT NULL
,  `alert` varchar(4) DEFAULT NULL
,  `price` double DEFAULT NULL
,  UNIQUE (`symbol`,`timeframe`,`datetime`,`alert`)
);
CREATE TABLE `intraday` (
  `symbol` varchar(12) DEFAULT NULL
,  `datetime` datetime DEFAULT NULL
,  `open` double DEFAULT NULL
,  `high` double DEFAULT NULL
,  `low` double DEFAULT NULL
,  `close` double DEFAULT NULL
,  `volume` integer  NOT NULL DEFAULT '0'
,  UNIQUE (`symbol`,`datetime`)
);
CREATE TABLE `operations` (
  `symbol` varchar(20) DEFAULT NULL
,  `date` date DEFAULT NULL
,  `type` char(1) DEFAULT NULL
,  `size` integer DEFAULT NULL
,  `price` float DEFAULT NULL
,  `cost` float DEFAULT NULL
);
CREATE TABLE `stockinfo` (
  `symbol` varchar(12) NOT NULL DEFAULT ''
,  `name` varchar(64) NOT NULL DEFAULT ''
,  `exchange` varchar(16) NOT NULL DEFAULT ''
,  `capitalisation` float DEFAULT NULL
,  `low_52weeks` float DEFAULT NULL
,  `high_52weeks` float DEFAULT NULL
,  `earnings` float DEFAULT NULL
,  `dividend` float DEFAULT NULL
,  `p_e_ratio` float DEFAULT NULL
,  `avg_volume` integer DEFAULT NULL
,  `active` integer DEFAULT '1'
,  PRIMARY KEY (`symbol`)
);
CREATE TABLE `stockprices` (
  `symbol` varchar(12) DEFAULT NULL
,  `date` date DEFAULT NULL
,  `previous_close` double DEFAULT NULL
,  `day_open` double DEFAULT NULL
,  `day_high` double DEFAULT NULL
,  `day_low` double DEFAULT NULL
,  `day_close` double DEFAULT NULL
,  `day_change` double DEFAULT NULL
,  `bid` float DEFAULT NULL
,  `ask` float DEFAULT NULL
,  `volume` integer DEFAULT NULL
,  UNIQUE (`symbol`,`date`)
);
