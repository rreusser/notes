'use strict';

var path = require( 'path' );
var childProcess = require( 'child_process' );
var util = require( '../util.js' );

var ID = 'tests';

function check( mod, opts ) {
	var results = [];
	var testPath = path.join( mod.dir, 'test', 'test.js' );
	var testContent = util.readFile( testPath );

	if ( !testContent ) {
		results.push( util.skip( ID + '.assertion-count', 'No test.js' ) );
		results.push( util.skip( ID + '.no-scaffold-markers', 'No test.js' ) );
		if ( opts && opts.coverage ) {
			results.push( util.skip( ID + '.coverage-line', 'No test.js' ) );
			results.push( util.skip( ID + '.coverage-branch', 'No test.js' ) );
		}
		return results;
	}

	// 1. Test file has >2 substantive assertions
	// Count assert.* calls (excluding assert.fail which are scaffold markers)
	var assertCalls = ( testContent.match( /assert\.(strictEqual|deepStrictEqual|ok|equal|notEqual|notStrictEqual|throws|doesNotThrow|rejects|doesNotReject|match|doesNotMatch)\s*\(/g ) || [] ).length;
	if ( assertCalls > 2 ) {
		results.push( util.pass( ID + '.assertion-count', 'Tests have >2 assertions (' + assertCalls + ')' ) );
	} else {
		results.push( util.fail(
			ID + '.assertion-count',
			'Tests have >2 assertions',
			1, [ 'test.js' ],
			'Only ' + assertCalls + ' assertion(s) — likely scaffold'
		));
	}

	// 2. No scaffold markers
	var scaffoldHits = util.grepFile( testPath, /assert\.fail\s*\(\s*['"]TODO/ );
	if ( scaffoldHits.length === 0 ) {
		results.push( util.pass( ID + '.no-scaffold-markers', 'No scaffold assertion markers' ) );
	} else {
		var locs = [];
		var i;
		for ( i = 0; i < scaffoldHits.length; i++ ) {
			locs.push( 'test.js:' + scaffoldHits[ i ].line );
		}
		results.push( util.fail( ID + '.no-scaffold-markers', 'No scaffold assertion markers', scaffoldHits.length, locs ) );
	}

	// 3. Coverage (opt-in, expensive)
	if ( opts && opts.coverage ) {
		try {
			var output = childProcess.execSync(
				'node --test --experimental-test-coverage ' + JSON.stringify( testPath ),
				{
					cwd: util.ROOT,
					encoding: 'utf8',
					timeout: 120000,
					stdio: [ 'pipe', 'pipe', 'pipe' ]
				}
			);
			var stderr = '';
			try {
				stderr = childProcess.execSync(
					'node --test --experimental-test-coverage ' + JSON.stringify( testPath ),
					{
						cwd: util.ROOT,
						encoding: 'utf8',
						timeout: 120000,
						stdio: [ 'pipe', 'pipe', 'pipe' ]
					}
				);
			} catch ( e2 ) {
				stderr = ( e2.stderr || '' ) + ( e2.stdout || '' );
			}
			var allOutput = output + '\n' + stderr;

			// Parse coverage from output — look for base.js line coverage
			var lineMatch = allOutput.match( /base\.js\s*\|\s*([\d.]+)/);
			var branchMatch = allOutput.match( /base\.js\s*\|[\s\d.]+\|\s*([\d.]+)/);
			var lineCov = lineMatch ? parseFloat( lineMatch[ 1 ] ) : -1;
			var branchCov = branchMatch ? parseFloat( branchMatch[ 1 ] ) : -1;

			if ( lineCov >= 90 ) {
				results.push( util.pass( ID + '.coverage-line', 'Line coverage >= 90% (' + lineCov.toFixed( 1 ) + '%)' ) );
			} else if ( lineCov >= 0 ) {
				results.push( util.fail(
					ID + '.coverage-line',
					'Line coverage >= 90%',
					1, [ 'base.js' ],
					'Line coverage: ' + lineCov.toFixed( 1 ) + '%'
				));
			} else {
				results.push( util.warn( ID + '.coverage-line', 'Line coverage >= 90%', 1, [], 'Could not parse coverage' ) );
			}

			if ( branchCov >= 85 ) {
				results.push( util.pass( ID + '.coverage-branch', 'Branch coverage >= 85% (' + branchCov.toFixed( 1 ) + '%)' ) );
			} else if ( branchCov >= 0 ) {
				results.push( util.fail(
					ID + '.coverage-branch',
					'Branch coverage >= 85%',
					1, [ 'base.js' ],
					'Branch coverage: ' + branchCov.toFixed( 1 ) + '%'
				));
			} else {
				results.push( util.warn( ID + '.coverage-branch', 'Branch coverage >= 85%', 1, [], 'Could not parse coverage' ) );
			}
		} catch ( e ) {
			results.push( util.warn( ID + '.coverage-line', 'Line coverage >= 90%', 1, [], 'Coverage run failed' ) );
			results.push( util.warn( ID + '.coverage-branch', 'Branch coverage >= 85%', 1, [], 'Coverage run failed' ) );
		}
	}

	return results;
}

module.exports = check;
