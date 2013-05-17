insert into users values
  (1,'Kakadu',     'kakadu@kakadu.net',     '123',0),
  (2,'Lerss',      'lerss@kakadu.net',      '123',0),
  (3,'Ckau',       'ckau@kakadu.net',       '123',0),
  (4,'Differentia','differentia@kakadu.net','123',0)
;

insert into friends values
       (1,2), (1,3), (1,4),
       (2,3), (2,1),
       (3,1), (3,2), (3,4)
;
