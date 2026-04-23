'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlagts = require( './dlagts.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlagts, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlagts;
