'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgebal = require( './zgebal.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgebal, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgebal;
