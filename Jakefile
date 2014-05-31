var util = require('util');
var fs = require('fs');

function getDateTime() {

    var date = new Date();

    var hour = date.getHours();
    hour = (hour < 10 ? "0" : "") + hour;

    var min  = date.getMinutes();
    min = (min < 10 ? "0" : "") + min;

    var sec  = date.getSeconds();
    sec = (sec < 10 ? "0" : "") + sec;

    var year = date.getFullYear();

    var month = date.getMonth() + 1;
    month = (month < 10 ? "0" : "") + month;

    var day  = date.getDate();
    day = (day < 10 ? "0" : "") + day;

    return year + "-" + month + "-" + day + " " + hour + ":" + min + ":" + sec;

}

desc('This is the default task.');
task('default', function () {
  console.log('This is the default task.');
});

namespace('post', function () {
  desc('This the 1000 task');
  task('1000', function (day) {
    var today = getDateTime();
    var date = today.split(" ").shift();
    var dir_path = "./src/documents/10000/" + date.replace(/-/gi, "/");
    var file_name = dir_path + "/8_1_" + day + ".html.md"
    jake.mkdirP(dir_path);

    fs.exists(file_name, function (exists) {
      if (exists) {
        util.error(file_name + " already exist!!");
      } else {
        var template = [ '---',
                         'title: "8차 천일 결사 1차 백일 기도 정진 ' + day + '일째"',
                         'date: ' + today,
                         'tags:',
                         '    - 8000th',
                         '    - 8-100th',
                         '    - ' + day + 'th',
                         '---',
                         '',
                         '#수행일지',
                         ''
                         ].join('\n');

        fs.writeFile(file_name, template, 'utf8', function (err) {
          if (err) return console.log(err);
          util.log('file created => ' + file_name);
        });

      }
    });

  });

  desc('This is for a new post');
  task('new', function (filename, title) {
    console.log(filename)
    console.log(title, filename)
    var today = getDateTime();
    var date = today.split(" ").shift();
    var dir_path = "./src/documents/posts/" + date.replace(/-/gi, "/");

    var file_name = dir_path + "/" + filename + ".html.md"

    jake.mkdirP(dir_path);

    fs.exists(file_name, function (exists) {
      if (exists) {
        util.error(file_name + " already exist!!");
      } else {
        var template = [ '---',
                         'title: "' + title + '"',
                         'date: ' + today,
                         'tags:',
                         '    - ',
                         '---',
                         '',
                         ''
                         ].join('\n');

        fs.writeFile(file_name, template, 'utf8', function (err) {
          if (err) return console.log(err);
          util.log('file created => ' + file_name);
        });

      }
    });

  });

  desc('This the foo:baz task');
  task('baz', ['default', 'foo:bar'], function () {
    console.log('doing foo:baz task');
  });

  desc('This is an awesome task.');
  task('awesome', function (a, b, c) {
    console.log(a, b, c);
  });
});