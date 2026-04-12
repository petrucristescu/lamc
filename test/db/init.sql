-- Test database initialization for Churing MySQL driver tests

CREATE TABLE IF NOT EXISTS users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    age INT NOT NULL,
    email VARCHAR(200)
);

INSERT INTO users (name, age, email) VALUES
    ('Alice', 30, 'alice@test.com'),
    ('Bob', 25, 'bob@test.com'),
    ('Charlie', 35, NULL);

CREATE TABLE IF NOT EXISTS products (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    price DECIMAL(10, 2) NOT NULL
);

INSERT INTO products (name, price) VALUES
    ('Widget', 9.99),
    ('Gadget', 24.50),
    ('Doohickey', 4.75);
