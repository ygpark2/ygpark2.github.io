---
title: Python pyenv, virtualenv, autoenv, venv 사용법 및 비교
date: 2017-01-08T10:19:57+09:00
published: true
tags: python, pyenv, virtualenv, autoenv, venv
---

# pyenv


# virtualenvwrapper

## 기본적인 설치법

  $ pip install virtualenvwrapper

or:

  $ sudo pip install virtualenvwrapper

<!--more-->

## 쉘 시작 파일

Add three lines to your shell startup file (.bashrc, .profile, etc.) to set the location where the virtual environments should live, the location of your development project directories, and the location of the script installed with this package:

다음 라인을 쉘 설정 파일(.bashrc, .profile, etc.)에 추가해 주세요.

  export WORKON_HOME=$HOME/.virtualenvs
  export PROJECT_HOME=$HOME/Devel
  source /usr/local/bin/virtualenvwrapper.sh
