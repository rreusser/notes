
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtplqt2 = require( './dtplqt2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtplqt2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtplqt2;
