
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtplqt = require( './dtplqt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtplqt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtplqt;
