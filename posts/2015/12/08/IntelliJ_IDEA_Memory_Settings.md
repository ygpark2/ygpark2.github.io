---
title: The One and Only Reason to Customize IntelliJ IDEA Memory Settings
date: 2015-12-08T01:15:17+09:00
published: true
tags: IntelliJ, Memory, Settings
---

http://tomaszdziurko.pl/2015/11/1-and-the-only-one-to-customize-intellij-idea-memory-settings/

위의 글을 번역했습니다.

# 인텔리제이 메모리 설정

## 목표

메모리 사용과 속도 개선을 위한 최적의 메모리 설정을 찾는 것입니다.

## jstat -gcutil

JDK에는 JVM과 Garbage Collector 통계를 모니터하기위한 툴입니다. 많은 옵션들이 있지만 우리는 여기서 다음의 옵션을 살펴보겠습니다.

-gcutil - Summary of garbage collection statistics.

S0: Survivor space 0 utilization as a percentage of the space's current capacity.
S1: Survivor space 1 utilization as a percentage of the space's current capacity.
E: Eden space utilization as a percentage of the space's current capacity.
O: Old space utilization as a percentage of the space's current capacity.
M: Metaspace utilization as a percentage of the space's current capacity.
CCS: Compressed class space utilization as a percentage.
YGC: Number of young generation GC events.
YGCT: Young generation garbage collection time.
FGC: Number of full GC events.
FGCT: Full garbage collection time.
GCT: Total garbage collection time.

이 명령어의 출력 결과는 다음과 같습니다.

S0     S1    E     O     M    CCS  YGC YGCT FGC  FGCT   GCT
89.70 0.00 81.26 74.27 95.68 91.76 40 2.444 14  0.715  3.159

이 글에서 가장 중요한 파라메터들은 GC 이벤트(YGC, GFC) 수이고 그리고 컬렉션 타임(YGCT, FGCT) 입니다.

### 기본 설정

-Xms128m
-Xmx750m
-XX:MaxPermSize=350m
-XX:ReservedCodeCacheSize=240m
-XX:+UseCompressedOops

### 대용량 설정(붉은색)

-Xms2g
-Xmx2g
-XX:ReservedCodeCacheSize=1024m
-XX:+UseCompressedOops
-server
-Xms2g
-Xmx2g
-XX:NewRatio=3
-Xss16m
-XX:+UseConcMarkSweepGC
-XX:+CMSParallelRemarkEnabled
-XX:ConcGCThreads=4
-XX:MaxPermSize=350m
-XX:ReservedCodeCacheSize=240m
-XX:+AlwaysPreTouch
-XX:+TieredCompilation
-XX:+UseCompressedOops
-XX:SoftRefLRUPolicyMSPerMB=50
-ea
-Dsun.io.useCanonCaches=false
-Djava.net.preferIPv4Stack=true
-Djsse.enableSNIExtension=false
-XX:+HeapDumpOnOutOfMemoryError
-XX:-OmitStackTraceInFastThrow
-Dawt.useSystemAAFontSettings=lcd
### 복잡한 설정(오렌지색)

-server
-Xms2g
-Xmx2g
-XX:NewRatio=3
-Xss16m
-XX:+UseConcMarkSweepGC
-XX:+CMSParallelRemarkEnabled
-XX:ConcGCThreads=4
-XX:ReservedCodeCacheSize=240m
-XX:+AlwaysPreTouch
-XX:+TieredCompilation
-XX:+UseCompressedOops
-XX:SoftRefLRUPolicyMSPerMB=50
-Dsun.io.useCanonCaches=false
-Djava.net.preferIPv4Stack=true
-Djsse.enableSNIExtension=false
-ea

이 설정값들은 bin 폴더안의 idea64.vmoptions파일에 있습니다.

### 실험결과

생략합니다.

## 요약

인텔리제이 메모리 설정을 조그만 바꿈으로서 아주 상당한 성능의 개선을 볼수있다. 물론 더 많은 메모리를 할당할수록 더 많은 성능의 개선을 기대할 수 있다. 하지만 우리는 메모리를 사용하는 많은 프로그램들과 함께 사용해야하므로 성능과 메모리 소비에서 최적의 설정값들을 찾는것이 이 글의 목적이다. 대부분의 경우에는 Xmx값을 2g 또는 3g 사이에서 설정할 경우 가장 최상의 성능을 기대할 수 있는것 같다. 만약 당신이 좀 더 여유 시간이 있으시다면 jstat 또는 jvisualm가지고 여러 VM flags값들을 설정하면서 최고의 성능을 낼 수 있는 설정을 찾으면 좋을것같다.



이글을 읽고 찾은 나의 설정 값

-server
-Xms2g
-Xmx2g
-XX:NewRatio=3
-Xss16m
-XX:+UseConcMarkSweepGC
-XX:+CMSParallelRemarkEnabled
-XX:ConcGCThreads=4
-XX:MaxPermSize=350m
-XX:ReservedCodeCacheSize=240m
-XX:+AlwaysPreTouch
-XX:+TieredCompilation
-XX:+UseCompressedOops
-XX:SoftRefLRUPolicyMSPerMB=50
-ea
-Dsun.io.useCanonCaches=false
-Djava.net.preferIPv4Stack=true
-Djsse.enableSNIExtension=false
-XX:+HeapDumpOnOutOfMemoryError
-XX:-OmitStackTraceInFastThrow
-Dawt.useSystemAAFontSettings=lcd
