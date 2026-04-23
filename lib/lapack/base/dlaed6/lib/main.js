
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-read-only-property' );
var dlaed6 = require( './dlaed6.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaed6, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaed6;
