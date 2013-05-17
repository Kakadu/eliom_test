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
