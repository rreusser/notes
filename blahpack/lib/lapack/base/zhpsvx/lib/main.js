
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhpsvx = require( './zhpsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhpsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhpsvx;
