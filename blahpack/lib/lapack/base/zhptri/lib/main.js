
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhptri = require( './zhptri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhptri, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhptri;
