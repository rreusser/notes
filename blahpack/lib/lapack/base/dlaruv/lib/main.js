'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaruv = require( './dlaruv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaruv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaruv;
