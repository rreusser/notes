
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhetri = require( './zhetri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetri, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetri;
