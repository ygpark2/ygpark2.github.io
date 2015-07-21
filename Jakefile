var util = require('util');
var fs = require('fs');

function getDateTime() {

    var now = new Date(),
        tzo = -now.getTimezoneOffset(),
        dif = tzo >= 0 ? '+' : '-',
        pad = function(num) {
            var norm = Math.abs(Math.floor(num));
            return (norm < 10 ? '0' : '') + norm;
        };
    return now.getFullYear()
        + '-' + pad(now.getMonth()+1)
        + '-' + pad(now.getDate())
        + 'T' + pad(now.getHours())
        + ':' + pad(now.getMinutes())
        + ':' + pad(now.getSeconds())
        + dif + pad(tzo / 60)
        + ':' + pad(tzo % 60);
}

desc('This is the default task.');
task('default', function () {
  console.log('This is the default task.');
});

namespace('doc', function () {
  desc('This the 1000 task');
  task('1000', function (thounsand, hundred, day) {
    var today = getDateTime();
    var date = today.split("T").shift();
    var dir_path = "./posts/" + date.replace(/-/gi, "/");
    var file_name = dir_path + "/" + thounsand + "_" + hundred + "_" + day + ".html.md"
    jake.mkdirP(dir_path);

    fs.exists(file_name, function (exists) {
      if (exists) {
        util.error(file_name + " already exist!!");
      } else {
        var template = [ '---',
                         'title: ' + thounsand + '차 천일 결사 ' + hundred + '차 백일 기도 정진 ' + day + '일째',
                         'date: ' + today,
                         'published: true',
                         'tags: 10000 결사, ' + thounsand + '000th, ' + thounsand + '-' + hundred + '00th, ' + day + 'th',
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
  task('post', function (filename, title) {
    var today = getDateTime();
    var date = today.split("T").shift();
    var dir_path = "./posts/" + date.replace(/-/gi, "/");

    var file_name = dir_path + "/" + filename + ".html.md"

    jake.mkdirP(dir_path);

    fs.exists(file_name, function (exists) {
      if (exists) {
        util.error(file_name + " already exist!!");
      } else {
        var template = [ '---',
                         'title: "' + title + '"',
                         'date: ' + today,
                         'published: true',
                         'tags: ',
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
