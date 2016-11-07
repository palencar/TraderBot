CREATE TABLE "operations" (
  "symbol" varchar(20) DEFAULT NULL,
  "date" date DEFAULT NULL,
  "type" char(1) DEFAULT NULL,
  "size" int(11) DEFAULT NULL,
  "price" float DEFAULT NULL,
  "cost" float DEFAULT NULL
);
CREATE TABLE "stockinfo" (
  "symbol" varchar(12) NOT NULL DEFAULT '',
  "name" varchar(64) NOT NULL DEFAULT '',
  "exchange" varchar(16) NOT NULL DEFAULT '',
  "capitalisation" float DEFAULT NULL,
  "low_52weeks" float DEFAULT NULL,
  "high_52weeks" float DEFAULT NULL,
  "earnings" float DEFAULT NULL,
  "dividend" float DEFAULT NULL,
  "p_e_ratio" float DEFAULT NULL,
  "avg_volume" int(11) DEFAULT NULL,
  "active" tinyint(1) DEFAULT '1',
  PRIMARY KEY ("symbol")
);
CREATE TABLE "stockprices_intraday" (
  "symbol" varchar(12) NOT NULL DEFAULT '',
  "datetime" datetime DEFAULT NULL,
  "previous_close" float DEFAULT NULL,
  "min_open" float DEFAULT NULL,
  "min_low" float DEFAULT NULL,
  "min_high" float DEFAULT NULL,
  "min_close" float DEFAULT NULL,
  "bid" float DEFAULT NULL,
  "ask" float DEFAULT NULL,
  "volume" int(11) DEFAULT NULL
);
CREATE INDEX "stockprices_intraday_stockprices_pkey" ON "stockprices_intraday" ("symbol","datetime");
CREATE TABLE stockprices (symbol varchar(12), date date, previous_close double, day_open double, day_high double, day_low double, day_close double, day_change double, bid float, ask float, volume int);
CREATE UNIQUE INDEX stockindex on stockprices(symbol, date);
