
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhpevx = require( './zhpevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhpevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhpevx;
