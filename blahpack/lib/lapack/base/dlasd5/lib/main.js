'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlasd5 = require( './dlasd5.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasd5, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasd5;
