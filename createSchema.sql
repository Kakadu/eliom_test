GRANT ALL PRIVILEGES ON DATABASE traktor TO kakadu;
SET client_encoding = 'UTF8';


drop table if exists users ;
create table users (
    id              integer not null,
    nick            text not null,
    friends         int[] not null default '{}',
    post_ids        int[] not null default '{}',
    email           text not null,
    password_digest text not null,
    exp             int not null default 0
);

drop table if exists posts;
create table posts (   
    id                  integer not null,
    user_id             integer not null,
    material_id         integer not null,
    exp                 integer not null,
    comments            text,
    date_of_creation    date
);

drop table if exists materials;
create table materials (
    id          integer not null,
    title       text not null,
    author      text not null,
    exp         integer not null,
    profit      integer not null,
    sort_id     integer not null,
    skill_id    integer not null,
    posts_ids   integer[]
);

drop table if exists users_skills;
create table users_skills (
    user_id         integer not null,
    skill_id        integer not null,
    exp             integer not null
);

drop table if exists skills;
create table skills (
    id              integer not null,
    descr           text,
    parent_id       integer not null,
    maxexp          integer not null
);

drop table if exists material_sorts;
create table material_sorts (
    id              integer not null,
    descr           text,
    action          text
);


