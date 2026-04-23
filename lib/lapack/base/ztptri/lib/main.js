
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztptri = require( './ztptri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztptri, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztptri;
