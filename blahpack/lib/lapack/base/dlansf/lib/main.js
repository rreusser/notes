
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlansf = require( './dlansf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlansf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlansf;
