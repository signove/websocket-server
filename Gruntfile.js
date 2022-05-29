module.exports = function(grunt) {

    // Project configuration.
    grunt.initConfig({
      pkg: grunt.file.readJSON('package.json'),
      concat: {
        dist: {
            src: 'js_lib/*.js',
            dest: 'rebar/priv/microservicehub/microservicehub.all.js' 
        }
      },
      uglify: {
        options: {
            banner: '/*! <%= pkg.name %> <%= grunt.template.today("yyyy-mm-dd") %> */\n',
            sourceMap : true
        },
        build: {
          src: 'rebar/priv/microservicehub/microservicehub.all.js',
          dest: 'rebar/priv/microservicehub/microservicehub.all.min.js'
        }
      },
      shell: {
        options: {
            stderr: false
        },
		js_doc: {
			command: './node_modules/.bin/jsdoc ./js_lib/*.js README.md --recurse --destination ./rebar/priv/microservicehub/doc'
		},
        js_copy_html: {
            command: 'cp -R html_samples/*.html rebar/priv/microservicehub/'
        },
        erlang_deps: {
            command: 'rebar3 get-deps',
            cwd: 'rebar'
        },
        erlang_build: {
            command: './compile.sh',
            cwd: 'rebar'
        },
        erlang_start: {
            command: './start.sh',
            cwd: 'rebar',
            options: {
                failOnError: false
            }
        },
        erlang_stop: {
            command: './stop.sh',
            cwd: 'rebar',
            options: {
                failOnError: false
            }
        }
	  }
    });
  
    grunt.loadNpmTasks('grunt-contrib-uglify');
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-shell');
  
    grunt.registerTask('default', ['build']);
    
    grunt.registerTask('js:build', ['concat', 'uglify', 'js:copy_html', 'js:doc']);

    grunt.registerTask('js:doc', ['shell:js_doc']);

    grunt.registerTask('js:copy_html', ['shell:js_copy_html']);

    grunt.registerTask('erlang:deps', ['shell:erlang_deps']);
    
    grunt.registerTask('erlang:build', ['shell:erlang_build']);

    grunt.registerTask('erlang:stop', ['shell:erlang_stop']);

    grunt.registerTask('erlang:start', ['shell:erlang_start']);

    grunt.registerTask('build', ['erlang:stop', 'js:build', 'erlang:build']);

    grunt.registerTask('run', ['build', 'erlang:start']);
  
  };