
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhpgvx = require( './zhpgvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhpgvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhpgvx;
