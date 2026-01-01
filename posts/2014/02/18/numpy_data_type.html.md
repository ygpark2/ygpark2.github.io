---
title: Numpy Data Type
date: "2014-02-18T13:57:00+09:00"
tags: numpy, python
---

# 배열 생성

## Introduction
5가지의 배열을 생성하기 위한 메커니즘

 * 다른 파이썬 데이터 구조( 리스트, 튜플)에서 변환
 * numpy 고유의 배열 생성 오브젝트( arrange, ones, zeros, etc )
 * 표준 또는 맞춤형 포멧 또는 디스크에서 배열 읽기
 * 스트링이나 버퍼를 통한 로우 바이트로 배열 생성
 * 특별한 라이브러리 함수의 사용
 * This section will not cover means of replicating, joining, or otherwise expanding or mutating existing arrays.
 Nor will it cover creating object arrays or record arrays.
 Both of those are covered in their own sections.
 *

## Converting Python array_like Objects to Numpy Arrays

In general, numerical data arranged in an array-like structure in Python can be converted to arrays through the use of the array() function. The most obvious examples are lists and tuples. See the documentation for array() for details for its use. Some objects may support the array-protocol and allow conversion to arrays this way. A simple way to find out if the object can be converted to a numpy array using array() is simply to try it interactively and see if it works! (The Python Way).

일반적으로, 파이썬에서 배열같은 구조의 숫자 데이터 배열은 array() 함수를

``` python
x = np.array([2,3,1,0])
x = np.array([2, 3, 1, 0])
x = np.array([[1,2.0],[0,0],(1+1j,3.)]) # note mix of tuple and lists, and types
x = np.array([[ 1.+0.j, 2.+0.j], [ 0.+0.j, 0.+0.j], [ 1.+1.j, 3.+0.j]])
```
