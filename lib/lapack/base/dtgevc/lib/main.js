
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtgevc = require( './dtgevc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtgevc, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtgevc;
