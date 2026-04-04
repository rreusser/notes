
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhbgv = require( './zhbgv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhbgv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhbgv;
