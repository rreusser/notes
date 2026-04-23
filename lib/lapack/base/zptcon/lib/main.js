'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zptcon = require( './zptcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zptcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zptcon;
