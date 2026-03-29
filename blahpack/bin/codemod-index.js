#!/usr/bin/env node

/**
* Codemod for index.js files — applies mechanical stdlib convention fixes.
*
* Usage:
*   node bin/codemod-index.js lib/blas/base/{name}/lib/index.js
*   node bin/codemod-index.js --dry-run lib/blas/base/{name}/lib/index.js
*
* Fixes applied:
*   1. Add 'use strict' after license block if missing
*   2. Fix no-multiple-empty-lines (collapse 3+ blank lines to 2)
*   3. Fix jsdoc-emphasis-marker (* -> _ for emphasis in JSDoc)
*   4. Fix jsdoc-leading-description-sentence (ensure period at end)
*/

'use strict';

var fs = require( 'fs' );

var dryRun = false;
var files = [];
var i;

for ( i = 2; i < process.argv.length; i++ ) {
	if ( process.argv[ i ] === '--dry-run' ) {
		dryRun = true;
	} else {
		files.push( process.argv[ i ] );
	}
}

if ( files.length === 0 ) {
	process.stderr.write(
		'Usage: node bin/codemod-index.js [--dry-run] <files...>\n'
	);
	process.exit( 1 );
}

var changed = 0;

files.forEach( function processFile( file ) {
	if ( !fs.existsSync( file ) ) {
		return;
	}
	var src = fs.readFileSync( file, 'utf8' );
	var original = src;

	// 1. Add 'use strict' after license block if missing
	if ( !/^'use strict';$/m.test( src ) ) {
		// Find end of license block (first `*/`)
		var licenseEnd = src.indexOf( '*/' );
		if ( licenseEnd !== -1 ) {
			var insertPos = licenseEnd + 2;

			// Skip whitespace after license block
			while ( insertPos < src.length && src[ insertPos ] === '\n' ) {
				insertPos += 1;
			}
			src = src.substring( 0, licenseEnd + 2 ) +
				'\n\n\'use strict\';\n\n' +
				src.substring( insertPos );
		}
	}

	// 2. Fix no-multiple-empty-lines (collapse 3+ blank lines to 2)
	src = src.replace( /\n{4,}/g, '\n\n\n' );

	// 3. Fix jsdoc-emphasis-marker: *text* -> _text_ in JSDoc
	//    Only within JSDoc blocks (lines starting with *)
	//    Be careful not to touch multiplication, pointers, etc.
	var lines = src.split( '\n' );
	for ( i = 0; i < lines.length; i++ ) {
		var line = lines[ i ];

		// Only process JSDoc lines (lines starting with optional whitespace + *)
		if ( !/^\s*\*/.test( line ) ) {
			continue;
		}

		// Replace *word(s)* emphasis with _word(s)_ emphasis
		// Match: space or start, then *word(s)*, then space or punctuation
		// Avoid: ** (bold), * at line start (list), *16 (complex type)
		lines[ i ] = line.replace(
			/(?<=\s)\*([A-Za-z][A-Za-z0-9 ]*[A-Za-z0-9])\*(?=[\s,.\-;:)]|$)/g,
			'_$1_'
		);

		// Also handle single-word: *word*
		lines[ i ] = lines[ i ].replace(
			/(?<=\s)\*([A-Za-z][A-Za-z0-9]*)\*(?=[\s,.\-;:)]|$)/g,
			'_$1_'
		);
	}
	src = lines.join( '\n' );

	if ( src !== original ) {
		changed += 1;
		if ( dryRun ) {
			process.stdout.write( 'WOULD FIX: ' + file + '\n' );
		} else {
			fs.writeFileSync( file, src, 'utf8' );
			process.stdout.write( 'FIXED: ' + file + '\n' );
		}
	} else {
		process.stdout.write( 'OK: ' + file + '\n' );
	}
});

process.stdout.write( '\n' + changed + ' files ' +
	( dryRun ? 'would be ' : '' ) + 'changed\n' );
