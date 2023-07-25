CREATE EXTENSION if not exists pgcrypto;

create table if not exists users (
    id uuid not null default gen_random_uuid(),
    username varchar not null unique,
    email varchar not null unique,
    password varchar not null,
    bio varchar,
    image varchar,
    followed_by uuid[] default '{}',
    primary key (id)
);

create table if not exists articles (
    id serial,
    slug varchar not null unique,
    title varchar not null,
    body text not null,
    description varchar,
    created_at timestamp with time zone default now(),
    updated_at timestamp with time zone default now(),
    favorited_by uuid[] default '{}',
    author_id uuid not null,
    tag_list varchar array default '{}',
    primary key (id),
    foreign key (author_id)
    references users (id)
);

create table if not exists comments (
    id serial,
    body text not null,
    author_id uuid not null,
    slug varchar not null,
    created_at timestamp with time zone default now(),
    updated_at timestamp with time zone default now(),
    primary key (id),
    foreign key (author_id)
    references users (id) on delete cascade,
    foreign key (slug)
    references articles (slug) on delete cascade
);
