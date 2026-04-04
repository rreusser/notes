'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlansf = require( './zlansf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlansf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlansf;
