
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhetf2rk = require( './zhetf2_rk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetf2rk, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetf2rk;
