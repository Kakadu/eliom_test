insert into users (id,nick,email,password_digest,exp) values
  (DEFAULT,'Kakadu',     'kakadu@kakadu.net',     '123',0),
  (DEFAULT,'Lerss',      'lerss@kakadu.net',      '123',0),
  (DEFAULT,'Ckau',       'ckau@kakadu.net',       '123',0),
  (DEFAULT,'Differentia','differentia@kakadu.net','123',0)
;

insert into friends values
       (1,2), (1,3), (1,4),
       (2,3), (2,1),
       (3,1), (3,2), (3,4)
;
/* adding skills */
INSERT INTO skills (id, descr, maxexp) VALUES
        (DEFAULT, 'Chess', 1000),
        (DEFAULT, 'Kings Gambit', 500),
        
        (DEFAULT, 'Music', 1000),
        (DEFAULT, 'Classic music', 300),

        (DEFAULT, 'IT',    2000),
        (DEFAULT, 'Software engeneering', 1000),

        (DEFAULT, 'Literature', 3000),
        (DEFAULT, 'Classic literature', 2000),
        (DEFAULT, 'Poetry',             2000)
        ;
INSERT INTO parent_skills (child_id, parent_id) VALUES
        (2,1),
        (4,3),
        (6,5),
        (8,7), (9,7)
;
/* adding maetrials for skills above */
INSERT INTO material_sorts (descr, action) VALUES /* I omit id here */
        ('book','book'),
        ('music', 'music')
;
INSERT INTO materials (title, author, exp, profit, sort_id, skill_id) VALUES
        ('Andersen-Kiseritsky. Immortal game', 'Andersen', 5, 5, 1, 2),
        ('Live concert in Saint-Petersburg on May 17, 2013', 'Tarja Turunen', 11, 11, 2, 3),
        ('Git manual', 'Git team', 1, 1, 1, 6),
        ('Anna Karenina','Lev Tolstoy', 10,10, 1, 8),
        ('I remember wonderful moment', 'Alexander Pushkin', 5, 5, 1, 9),
        ('Kuznetchik', 'hz', 100,100, 2, 3)
;

/* posts */
INSERT INTO posts (user_id, material_id, exp, date_of_creation, comments) VALUES
        (1, 2,10, DATE '2013-05-17 10:23:54', 'Visited concert of Tarja Turunen'),
        (2, 1, 1, DATE '2013-01-01 10:23:54', 'Studied Immortal Game Andersen vs. Kiseritsky'),
        (4, 1,99, DATE '2013-01-01 10:23:54', 'Studied Immortal Game Andersen vs. Kiseritsky'),
        (1, 3, 1, DATE '2013-04-04 10:23:54', 'Found something useful in Git manual'),
        (1, 5, 4, DATE '2013-05-05 10:23:54', 'Studied a poem by heart =)'),
        (1, 4,30, DATE '2013-06-06 10:23:54', 'Finished reading Anna Karenina. Sad. TT'),
        (3, 6,99, DATE '2013-02-02 10:23:54', 'I can play it on my guitar')
;




