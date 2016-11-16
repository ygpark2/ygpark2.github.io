---
title: nodejs fs module 소개
date: "2014-04-06T20:51:39+09:00"
tags: nodejs, fs
---

Today I felt very annoying to make a directory every day to create a new post. Because how my docpad blog system configured is to save each post into the folder named according to date information. For example today is 2014-04-05 then the directory structure will be like 2014/04/05/created_file. Hence I need to create day folder everyday since day will be chanaged everyday. If month changed then I also need to create month folder.
Therefore I want to automate the whole process to make the procedure simple.
The first thing I have done is to choose a proper task program
What I try to do is to make a recursive directory according to today date.

~~~~~nodejs
dirs.map(function(dir) {
  dir_path += "/" + dir
  console.log("loop => " + dir_path);
  fs.mkdir(dir_path, 0755, true, function (err) {
    if (err) {
      console.log(err);
    } else {
      console.log('Directory ' + directory + ' created.');
    }
  });
});
~~~~~
