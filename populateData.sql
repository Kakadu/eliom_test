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
        (1, 'Chess', 1000),
        (2, 'Kings Gambit', 500),

        (200, 'Music', 1000),
        (201, 'Classic music', 300),

        (400, 'IT',    2000),
        (401, 'Software engeneering',               1000),
        (402, 'Project mangement',                  1000),
        (420, 'Linux',                              1000),
        (421, 'Linux kernel development',            500),
        (450, 'Windows',                            1000),
        (460, 'Programming languages',              5000),
        (461, 'C++',                                 100),
        (462, 'Windows development in C++',           20),
        (463, 'Cross-platform development',          500),

        (475, 'OCaml',                               200),
        (476, 'Web-develpment',                       50),

        (600, 'Literature', 3000),
        (601, 'Classic literature', 2000),
        (602, 'Poetry',             2000),

        (700, 'Cybersport',         1000),
        (701, 'StarCraft2 II',       500),

        (99999, 'Unknown', 99999)
        ;
INSERT INTO parent_skills (child_id, parent_id) VALUES
        (  2,  1),
        (201,200),
        (401,400), (420,400), (450,400), (460,400),
        /* SE */   (402,401),
        /* Linux*/ (421,420),
        /*windows*/
        /*langs */ (461,460), (462,461), (463,461), (475, 460), (476,475),
        (602,600), (601,600),
        /*Cybersport */ (701,700)
;
/* adding maetrials for skills above */
INSERT INTO material_sorts (descr, action) VALUES /* I omit id here */
        ('book','book'),
        ('music', 'music'),
        ('achievement', 'achievement')
;
INSERT INTO materials (title, author, exp, profit, sort_id, skill_id) VALUES
        ('Andersen-Kiseritsky. Immortal game', 'Andersen',                     5,  5,  1,    2),
        ('Live concert in Saint-Petersburg on May 17, 2013', 'Tarja Turunen', 11, 11,  2,  200),
        ('Git manual', 'Git team',                                             1,  1,  1,  401),
        ('Anna Karenina','Lev Tolstoy',                                       10, 10,  1,  601),
        ('I remember wonderful moment', 'Alexander Pushkin',                   5,  5,  1,  602),
        ('Kuznetchik', 'hz',                                                 100,100,  2,  200),
        ('токката-фуга', 'Бах Иоганн Себастьян',                              30, 30,  2,  200),
        ('','',        /* Starcraft II */                                   1000,999,  3,  701)
;

/* posts */
INSERT INTO posts (user_id, material_id, exp, date_of_creation, action_text, comments) VALUES
        (1, 2, 10, DATE '2013-05-17 10:23:54', 'visited', 'Visited concert of Tarja Turunen'),
        (2, 1,  1, DATE '2013-01-01 10:23:54', 'studied', 'Immortal Game Andersen vs. Kiseritsky'),
        (4, 1, 99, DATE '2013-01-01 10:23:54', 'studied', 'Immortal Game Andersen vs. Kiseritsky'),
        (1, 3,  1, DATE '2013-04-04 10:23:54', 'studied', 'Found something useful in Git manual'),
        (1, 5,  4, DATE '2013-05-05 10:23:54', 'studied', 'Studied a poem by heart =)'),
        (1, 4, 30, DATE '2013-06-06 10:23:54', 'studied', 'Finished reading Anna Karenina. Sad. TT'),
        (3, 6, 99, DATE '2013-02-02 10:23:54', 'studied', 'I can play it on my guitar'),
        (2, 7, 38, DATE '2013-06-06 11:00:11', 'studied', 'I can play it!'), /* токката-фуга*/
        (4, 8,400, DATE '2013-06-06 11:40:01', 'achievement', 'Graduated to Master league in SC2')
;
