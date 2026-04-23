
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zrscl = require( './zrscl.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zrscl, 'ndarray', ndarray );


// EXPORTS //

module.exports = zrscl;
