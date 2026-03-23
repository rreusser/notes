

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlaset = require( './dlaset.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaset, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaset;
