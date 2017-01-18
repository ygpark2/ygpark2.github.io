---
title: Elasticsearch Reference
date: 2016-12-13T11:17:11+09:00
published: true
tags: elasticsearch, document
---


# Document

## API Conventions
elasticsearch REST API는 HTTP위에서 JSON을 사용합니다.

달리 명시되지 아니하는 한, 이 장에서 사용하는 컨벤션은 REST API를 통해서

<!--more-->

### Multiple Indices

Most APIs that refer to an index parameter support execution across multiple indices, using simple test1,test2,test3 notation (or _all for all indices). It also support wildcards, for example: test* or *test or te*t or *test*, and the ability to "add" (+) and "remove" (-), for example: +test*,-test3.

모든 멀티 인덱스 API들은 다음의 url query 문자열 파라메터를 지원한다:
이것은 또한 와일드카드도 지원한다. 예를 들면, test*, *test, te*t, *test* 그리고 플러스(+), 마이너스(-) 기호도 지원한다. 예를 들면, +test*, -test3


* ignore_unavailable
지정된 인덱스들이 가용하지 않을때 무시할지 말지를 정하는 제어 지시자. 이것은 또한 존재하지 않거나 닫힌 인덱스들도 포함한다. true 또는 false값을 가질 수 있다.

* allow_no_indices
만약 와일드카드 인덱스 표현에 맞는 인덱스를 찾지 못하면 실패하기를 나타내는 제어지시자. true 또는 false값을 가질 수 있다. 예를 들면, 만약 와일드카드 foo*로 지정하고, foo로 시작하는 인덱스가 없을때, 이 설정값에따라 리퀘스트가 실패할것입니다. 이 설정은 또한 _all, * 또는 인덱스가 설정되어있지 않을경우에도 사용가능하다. 이 설정은 또한 알리아스가 닫힌 인덱스를 지정할때도 적용할 수 있다.

expand_wildcards

Controls to what kind of concrete indices wildcard indices expression expand to. If open is specified then the wildcard expression is expanded to only open indices and if closed is specified then the wildcard expression is expanded only to closed indices. Also both values (open,closed) can be specified to expand to all indices.
If none is specified then wildcard expansion will be disabled and if all is specified, wildcard expressions will expand to all indices (this is equivalent to specifying open,closed).

위의 파라메터에 대한 기본설정은 상용하는 api에 달려있다.

Note
단일 인덱스 API들, 예를 들면, 도큐먼트 API들 그리고 단일 인덱스 별칭 API들은 멀티 인덱스를 지원하지 않는다.

### Date math support in index names
Date math index name resolution enables you to search a range of time-series indices, rather than searching all of your time-series indices and filtering the results or maintaining aliases. Limiting the number of indices that are searched reduces the load on the cluster and improves execution performance. For example, if you are searching for errors in your daily logs, you can use a date math name template to restrict the search to the past two days.
Date math 인덱스 이름 해결책은 당신의 모든 시계열 인덱스들 검색하고 결과들을 필터링 또는

Almost all APIs that have an index parameter, support date math in the index parameter value.

A date math index name takes the following form:

<static_name{date_math_expr{date_format|time_zone}}>
Where:

static_name

is the static text part of the name

date_math_expr

is a dynamic date math expression that computes the date dynamically

date_format

is the optional format in which the computed date should be rendered. Defaults to YYYY.MM.dd.

time_zone

is the optional time zone . Defaults to utc.

You must enclose date math index name expressions within angle brackets, and all special characters should be URI encoded. For example:

```
# GET /<logstash-{now/d}>/_search
GET /%3Clogstash-%7Bnow%2Fd%7D%3E/_search
{
  "query" : {
    "match": {
      "test": "data"
    }
  }
}
```

COPY AS CURLVIEW IN CONSOLE
Note
Percent encoding of date math characters
The special characters used for date rounding must be URI encoded as follows:

Markdown | Less | Pretty
--- | --- | ---
*Still* | `renders` | **nicely**
1 | 2 | 3

< | %3C
--- | ---
> | %3E
/ | %2F
{ | %7B
} | %7D
*|* | %7C
+ | %2B
: | %3A

The following example shows different forms of date math index names and the final index names they resolve to given the current time is 22rd March 2024 noon utc.

Expression	Resolves to
<logstash-{now/d}>

logstash-2024.03.22

<logstash-{now/M}>

logstash-2024.03.01

<logstash-{now/M{YYYY.MM}}>

logstash-2024.03

<logstash-{now/M-1M{YYYY.MM}}>

logstash-2024.02

<logstash-{now/d{YYYY.MM.dd|+12:00}}>

logstash-2024.03.23

To use the characters { and } in the static part of an index name template, escape them with a backslash \, for example:

<elastic\\{ON\\}-{now/M}> resolves to elastic{ON}-2024.03.01
The following example shows a search request that searches the Logstash indices for the past three days, assuming the indices use the default Logstash index name format, logstash-YYYY.MM.dd.

# GET /<logstash-{now/d-2d}>,<logstash-{now/d-1d}>,<logstash-{now/d}>/_search
GET /%3Clogstash-%7Bnow%2Fd-2d%7D%3E%2C%3Clogstash-%7Bnow%2Fd-1d%7D%3E%2C%3Clogstash-%7Bnow%2Fd%7D%3E/_search
{
  "query" : {
    "match": {
      "test": "data"
    }
  }
}

### Common options


### URL-based access control
