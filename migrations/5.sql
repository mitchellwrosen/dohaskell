CREATE TABLE "list"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,CONSTRAINT "unique_list" UNIQUE ("name"));
CREATE TABLE "list_item"("id" INTEGER PRIMARY KEY,"user_id" INTEGER NOT NULL REFERENCES "user","list_id" INTEGER NOT NULL REFERENCES "list","res_id" INTEGER NOT NULL REFERENCES "resource","timestamp" TIMESTAMP NOT NULL,CONSTRAINT "unique_list_item" UNIQUE ("user_id","list_id","res_id"));
INSERT INTO "list"("id", "name") VALUES (1, "grokked");
INSERT INTO "list"("id", "name") VALUES (2, "favorites");
INSERT INTO "list_item"("user_id", "list_id", "res_id", "timestamp") SELECT user_id, 1, res_id, timestamp FROM "grokked";
INSERT INTO "list_item"("user_id", "list_id", "res_id", "timestamp") SELECT user_id, 2, res_id, timestamp FROM "favorite";
DROP TABLE "grokked";
DROP TABLE "favorite";
CREATE TABLE "comment"("id" INTEGER PRIMARY KEY,"res_id" INTEGER NOT NULL REFERENCES "resource","parent_id" INTEGER NULL REFERENCES "comment","user_id" INTEGER NOT NULL REFERENCES "user","body" VARCHAR NOT NULL,"timestamp" TIMESTAMP NOT NULL);
