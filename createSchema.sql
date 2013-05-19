
GRANT ALL PRIVILEGES ON DATABASE traktor TO kakadu;
SET client_encoding = 'UTF8';

/* BIGSERIAL forces creation of implicit sequence operators */
drop table if exists users;
create table users (
    id              BIGSERIAL NOT NULL PRIMARY KEY,
    nick            text NOT NULL,
    email           text NOT NULL,
    password_digest text NOT NULL,
    exp             int NOT NULL default 0
    /* add rules */
);
/* table for storing friend in many-to-many mode */
DROP TABLE IF EXISTS friends;
CREATE TABLE friends (
    user_id             BIGINT NOT NULL,
    friend_id           BIGINT NOT NULL
);

drop table if exists posts;
create table posts (
    id                  BIGSERIAL NOT NULL PRIMARY KEY,
    user_id             integer NOT NULL,
    material_id         integer NOT NULL,
    exp                 integer NOT NULL,
    comments            text,
    date_of_creation    timestamp
);

drop table if exists materials;
create table materials (
    id          BIGSERIAL NOT NULL PRIMARY KEY,
    title       text NOT NULL,
    author      text NOT NULL,
    exp         integer NOT NULL,
    profit      integer NOT NULL,
    sort_id     bigint NOT NULL,
    skill_id    integer NOT NULL
/*    posts_ids   integer[] */
);

drop table if exists users_skills;
create table users_skills (
    user_id         BIGSERIAL NOT NULL PRIMARY KEY,
    skill_id        integer NOT NULL,
    exp             integer NOT NULL
    /* add postive constraint using
     * CONSTRAINT positive_exp CHECK (exp>0)
    */
);

drop table if exists skills;
CREATE TABLE skills (
    id              BIGSERIAL NOT NULL PRIMARY KEY,
    descr           text,
    maxexp          integer NOT NULL
);

DROP TABLE IF EXISTS parent_skills;
CREATE TABLE parent_skills (
    child_id        BIGINT NOT NULL,
    parent_id       BIGINT
);

drop table if exists material_sorts;
create table material_sorts (
    id              BIGSERIAL NOT NULL PRIMARY KEY,
    descr           text,
    action          text
);
