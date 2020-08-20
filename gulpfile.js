var gulp = require('gulp-help')(require('gulp'), {hideEmpty : true, hideDepsMessage : true});
var babel = require('gulp-babel');
var shell = require('gulp-shell');
var concat = require('gulp-concat');
var uglify = require('gulp-uglify');
var runSequence = require('run-sequence');

gulp.task('default', ['help'], function() {

});


gulp.task('run', 'Stop the Erlang project, build it and start it again', function(cb) {
  runSequence('erlang:stop', 'js:build', 'erlang:build', 'erlang:start', cb);
});

gulp.task('js:copy_html', function() {
	return gulp.src('html_samples/*.html')
		.pipe(gulp.dest('rebar/priv/multiscreen/'));
});

gulp.task('js:build', 'Deploy Javascript into Erlang folder', ['js:copy_html', 'js:doc'], function() {
	return gulp.src('js_lib/*.js')
		.pipe(babel())
		.pipe(concat('multiscreen.all.min.js'))
		.pipe(uglify())
		.pipe(gulp.dest('rebar/priv/multiscreen/'));
});

gulp.task('js:doc', 'Generate Javascript Documentation', function() {
	return gulp.src('')
		.pipe(shell([
			'./node_modules/.bin/jsdoc ./js_lib/*.js README.md --recurse --destination ./rebar/priv/multiscreen/doc'
		], {
			'ignoreErrors' : true
		}));
});

gulp.task('erlang:deps', 'Get Erlang dependencies', function() {
	return gulp.src('')
		.pipe(shell([
			'rebar3 get-deps'
		], {
			'cwd' : 'rebar',
			'ignoreErrors' : true
		}));
});

gulp.task('erlang:build', 'Build Erlang project', function() {
	return gulp.src('')
		.pipe(shell([
			'./compile.sh'
		], {
			'cwd' : 'rebar',
			'ignoreErrors' : true
		}));
});

gulp.task('erlang:start', 'Start Erlang project (Start Web Server)', function() {
	return gulp.src('')
		.pipe(shell([
			'./start.sh'
		], {
			'cwd' : 'rebar',
			'ignoreErrors' : true
		}));
});

gulp.task('erlang:stop', 'Stop Erlang project (Stop Web Server)', function() {
	return gulp.src('')
		.pipe(shell([
			'./stop.sh'
		], {
			'cwd' : 'rebar',
			'ignoreErrors' : true
		}));
});
