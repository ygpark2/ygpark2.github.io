---
title: Solr 클라우드 설치
date: 2017-02-05T03:20:21+09:00
published: true
toc: true
tags: Solr, Cloud, Installation
---


## Infrastructure
* 리눅스 또는 유닉스 3대
* ELB 1
* 각각의 서버에 cl-solr1, cl-solr2, cl-solr3으로 cname을 생성하여 나중에 dns또는 hosts를 통하여 찾을 수 있도록 설정

<!--more-->

## Installations
1. zookeeper 설치
2. Solr 설치
3. JDK 설치

## Setup

- JAVA_HOME 환경 변수 설정

- cl-solr1,cl-solr2,cl-solr3에서 zoo.cfg 파일을 < zookeeper 설치 디렉토리 >/conf 경로 아래에 다음 내용으로 생성

    ```
    tickTime=2000
    dataDir=/var/lib/zookeeper/data
    clientPort=2181
    initLimit=5
    syncLimit=2
    server.1=cl-solr1:2888:3888
    server.2=cl-solr2:2888:3888
    server.3=cl-solr3:2888:3888
    ```

- 다음 명령어로 myid 파일을 zookeeper 서버 cl-solr1, cl-solr2 & cl-solr3에 각각 생성

    ```
    mkdir -p /var/lib/zookeeper/data/
    echo 1 > /var/lib/zookeeper/data/myid --1 for cl-solr1 and 2 for cl-solr2 ..
    ```

## zookeeper 시작하기
    < zookeeper 설치 디렉토리 >/bin/zkServer.sh start
    < zookeeper 설치 디렉토리 >/bin/zkServer.sh status

### 다음 명령어로 상태확인
    echo stat | nc cl-solr1 2181

## SOLR 시작하기
    < solr 설치 디렉토리 >/bin/solr start -c -z cl-solr1:2181,cl-solr2:2181,cl-solr3:2181 -h cl-solr1
    < solr 설치 디렉토리 >/bin/solr start -c -z cl-solr1:2181,cl-solr2:2181,cl-solr3:2181 -h cl-solr2
    < solr 설치 디렉토리 >/bin/solr start -c -z cl-solr1:2181,cl-solr2:2181,cl-solr3:2181 -h cl-solr3

## 새로운 컬렉션 생성
1. solr 노드중 한곳에서 다음 명령을 실행

    ```
    mkdir -p < solr 설치 디렉토리 >/server/solr/pats/conf
    ```

2. 현제 시스템에서 conf 폴더를 복사

    ```
    < solr 설치 디렉토리 >/bin/solr create -c my_collection_name -d < solr 설치 디렉토리 >/server/solr/pats/conf -n myname_cfg -shards 2 -replicationFactor 2
    ```
