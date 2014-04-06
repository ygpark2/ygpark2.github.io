
var fs = require('fs');

desc('This is the default task.');
task('default', function () {
  console.log('This is the default task.');
});

namespace('post', function () {
  desc('This the 1000 task');
  task('1000', function (day) {
    var today = new Date();
    var date = today.toISOString().split("T").shift();
    var dir_path = "./src/documents/posts/" + date.replace(/-/gi, '/');
    jake.mkdirP(dir_path);

    var template = [ '---',
                     'title: "8차 천일 결사 1차 백일 기도 정진 ' + day + '일째"',
                     'date: ' + date + ' ' + today.toLocaleTimeString(),
                     'tags:',
                     '    - 8000th',
                     '    - 8-100th',
                     '    - ' + day + 'th',
                     '---',
                     '',
                     '#수행일지',
                     ''
                   ].join('\n');
    fs.writeFile(dir_path + '/8_1_' + day + '.html.md', template, function (err) {
      if (err) return console.log(err);
      console.log('file created => ' + dir_path + '/8_1_' + day + '.html.md');
    });
  });

  desc('This the foo:bar task');
  task('new', function (title, filename) {
    var today = new Date();
    var date = today.toISOString().split("T").shift();
    var dir_path = "./src/documents/posts/" + date.replace(/-/gi, '/');
    jake.mkdirP(dir_path);
    var template = [ '---',
                     'title: "' + title + '"',
                     'date: ' + date + ' ' + today.toLocaleTimeString(),
                     'tags:',
                     '    - ',
                     '---',
                     '',
                     ''
                   ].join('\n');
    fs.writeFile(dir_path + '/' + filename +'.html.md', template, function (err) {
      if (err) return console.log(err);
      console.log("file name => " + dir_path + '/' + filename +'.html.md' + " created!");
    });
  });

  desc('This the foo:baz task');
  task('baz', ['default', 'foo:bar'], function () {
    console.log('doing foo:baz task');
  });

});