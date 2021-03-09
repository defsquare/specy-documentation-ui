(ns specy.documentation.pages
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.dom :as rdom]))


(defmulti schema->hiccup (fn [m opts] (cond
                                        (some? (:enum m)) :enum
                                        (some? (:$ref m)) :ref
                                        (= (:type m) "object") :object
                                        (= (:type m) "array") :array
                                        :else :default)))

(defmethod schema->hiccup :object [{:keys [required properties]} opts]
  (conj [:div "{"]
        (map (fn [[pname pschema]]
               [:div {:key (str pname "-schema-hiccup") :class (str "bg-gray-50 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-4" (if (contains? (set required) (some-> pname name)) "font-semibold" ""))}
                [:dt {:class "text-sm font-medium text-gray-500"} pname]
                [:dd {:class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"} (schema->hiccup pschema opts)]])
             (sort-by #(not (contains? (set required) (some-> (first %) name))) properties))
        "}"))


(defmethod schema->hiccup :array [{:keys [items]} opts]
  [:div {:class "sm:grid sm:grid-cols-5 sm:gap-4"}
   [:dt {:class "text-sm"} "List of"]
   [:dd {:class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-4"} (schema->hiccup items opts)]])

(defmethod schema->hiccup :ref [{:keys [$ref]} {:keys [on-click-ref-fn] :as opts}]
  (let [[_ ref-to-ns ref-to-name] (re-find #"\#/definitions/:([^\/]*)/(.*)" $ref)
        ref (keyword ref-to-ns ref-to-name)]
    [:span {:class   "inline-flex items-center rounded-full text-xs font-medium underline text-purple-500 w-full px-1 cursor-pointer"
            :onClick (fn [] (on-click-ref-fn ref))}
     [:div {:class "overflow-hidden truncate" :style {:direction "rtl" :text-align "left"}} (str ref-to-ns "/" ref-to-name)]]))

(defmethod schema->hiccup :enum [{:keys [enum]} opts]
  [:div {:class "sm:grid sm:grid-cols-5 sm:gap-4"}
   [:dt {:class "text-sm"} "One of"]
   [:dd {:class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-4 pl-4"}
    [:ul {:class "list-disc"}
     (doall (map (fn [v] [:li {:key (str "list-" v) :class ""} v]) enum))]]])

(defmethod schema->hiccup :default [{:keys [type]} opts]
  [:div type])

(defn- type-tag [{:keys [kind] :as building-block}]
  (condp = (keyword kind)
    :referential
    [:span {:class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-purple-100 text-purple-800 capitalize"} "referential"]

    :command
    [:span {:class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800 capitalize"} "command"]

    :query
    [:span {:class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800 capitalize"} "query"]

    :event
    [:span {:class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800 capitalize"} "event"]

    :entity
    [:span {:class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-orange-100 text-orange-800 capitalize"} "entity"]

    :value
    [:span {:class "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800 capitalize"} "value"]))

(defn- ->card [building-block {:keys [on-click-fn] :as opts}]
  [:div {:class "bg-white overflow-hidden shadow rounded-lg hover:shadow-2xl cursor-pointer" :onClick (fn [_] (on-click-fn))}
   [:div {:class "p-5"}
    [:div {:class "flex"}
     [:div {:class "ml-5 w-0 flex-1"}
      [:dl
       [:dt {:class "text-lg font-medium text-gray-900 truncate"} (:name building-block)]
       [:dd
        [:div {:class "text-sm font-medium text-gray-500 flex-none h-32"} (:doc building-block)]]
       [:dd [:div {:class "mt-2"} (type-tag building-block)]]]]]]
   [:div {:class "bg-gray-50 px-5 py-3"}
    [:div {:class "text-sm"}
     [:a {:href "#", :class "font-medium text-cyan-700 hover:text-cyan-900"} "Details"]]]])


(defn building-block-panel [building-block {:keys [close-fn on-click-ref-fn] :as opts}]
  [:div {:class "fixed inset-0 overflow-hidden h-screen"}
   [:div {:class "absolute inset-0 overflow-hidden"}
    [:section {:class "absolute inset-y-0 right-0 max-w-full flex", :aria-labelledby "slide-over-heading"}
     ;"<!--\n        Slide-over panel, show/hide based on slide-over state.
     ;        Entering: \"transform transition ease-in-out duration-500 sm:duration-700\"
     ; From: \"translate-x-full\"To: \"translate-x-0\"Leaving: \"transform transition ease-in-out duration-500 sm:duration-700\"From: \"translate-x-0\"To: \"translate-x-full\"-->"
     [:div {:class "bg-gray-100 bg-opacity-50 flex-initial w-screen" :onClick (fn [_] (close-fn))}]
     [:div {:class "flex-initial w-2/3"}
      [:div {:class "h-full flex flex-col py-6 bg-white shadow-xl overflow-y-scroll"}
       [:div {:class "px-4 sm:px-6"}
        [:div {:class "flex items-start justify-between"}
         [:h2 {:id "slide-over-heading", :class "text-lg font-medium text-gray-900"} (:name building-block)]
         [:div {:class "ml-3 h-7 flex items-center"}
          [:button {:class "bg-white rounded-md text-gray-400 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500" :onClick (fn [_] (close-fn))}
           [:span {:class "sr-only"} "Close panel"]
           ;"<!-- Heroicon name: outline/x -->"
           [:svg {:class "h-6 w-6", :xmlns "http://www.w3.org/2000/svg", :fill "none", :viewBox "0 0 24 24", :stroke "currentColor", :aria-hidden "true"}
            [:path {:stroke-linecap "round", :stroke-linejoin "round", :stroke-width "2", :d "M6 18L18 6M6 6l12 12"}]]]]]]
       [:div {:class "mt-6 relative flex-1 px-4 sm:px-6"}
        ;"<!-- Replace with your content -->"
        [:div {:class "bg-white shadow overflow-hidden sm:rounded-lg"}
         [:div {:class "border-t border-gray-200"}
          [:dl
           [:div {:class "bg-gray-50 px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"}
            [:dt {:class "text-sm font-medium text-gray-500"} "Name"]
            [:dd {:class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"} (:name building-block)]]
           [:div {:class "bg-white px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"}
            [:dt {:class "text-sm font-medium text-gray-500"} "Namespace"]
            [:dd {:class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"} (:ns building-block)]]
           [:div {:class "bg-gray-50 px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"}
            [:dt {:class "text-sm font-medium text-gray-500"} "Kind"]
            [:dd {:class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"} (type-tag building-block)]]
           [:div {:class "bg-white px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"}
            [:dt {:class "text-sm font-medium text-gray-500"} "Documentation"]
            [:dd {:class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"} (:doc building-block)]]
           [:div {:class "bg-gray-50 px-4 py-5 sm:grid sm:gap-4 sm:px-6"}
            [:dt {:class "text-sm font-medium text-gray-500"} "Schema"]
            [:dd {:class "mt-1 text-sm text-gray-900"}
             (schema->hiccup (:schema building-block) {:on-click-ref-fn on-click-ref-fn})]]]]]
        ;"<!-- /End replace -->"
        ]]]]]])

(defn home-page [context]
  (let [state (reagent/atom {:selected-building-block-panel nil})]
    (fn []
      [:div
       [:div.flex.overflow-hidden.h-screen.bg-gray-100
        ;; menu
        [:div.hidden.md:flex.md:flex-shrink-0
         [:div.flex.flex-col.w-64
          [:div.flex.items-center.h-16.flex-shrink-0.px-4.bg-gray-900
           ;;TODO Specy LOGO
           ]
          [:div.h-0.flex-1.flex.flex-col.overflow-y-auto
           [:nav.flex-1.px-2.py-4.bg-gray-800
            [:a.mt-1.group.flex.items-center.px-2.py-2.text-sm.leading-5.font-medium.text-gray-300.rounded-md.hover:text-white.hover:bg-gray-700.focus:outline-none.focus:text-white.focus:bg-gray-700.transition.ease-in-out.duration-150 {:href "#building-blocks"}
             [:svg.mr-3.h-6.w-6.text-gray-300.group-hover:text-gray-300.group-focus:text-gray-300.transition.ease-in-out.duration-150 {:xmlns "http://www.w3.org/2000/svg", :fill "none", :viewBox "0 0 24 24", :stroke "currentColor"}
              [:path {:stroke-linecap "round", :stroke-linejoin "round", :stroke-width "2", :d "M4 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2V6zM14 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V6zM4 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2v-2zM14 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z"}]]
             "Building blocks"]]]
          ;; defsquare logo
          [:div {:class "bg-gray-800"}
           [:svg {:y "0px", :xmlSpace "preserve", :viewBox "0 0 600 147", :xmlns "http://www.w3.org/2000/svg", :style {:enable-background "new 0 0 600 147" ".bracket" "#bf3d5e" ".text" "#FFFFFF"} :xmlnsXlink "http://www.w3.org/1999/xlink", :id "Layer_1", :x "0px", :version "1.1"}
            [:style "\t.st0{fill:#BF3D5E;}\n\t.st1{fill:#F7FAFC;}"]
            [:path {:class "st0", :d "M20.3,29.3h30.1v9H30.5v67.2h19.9v8.7H20.3V29.3z"}]
            [:path {:class "st1", :d "M110.9,99.6c0,0.8,0.1,1.7,0.2,2.6c0.2,1,0.4,1.9,0.6,2.9h-10.5c-0.4-0.3-0.7-0.7-0.8-1.3 c-0.1-0.6-0.2-1.1-0.2-1.6v-1.4c-0.5,0.9-1.2,1.8-2.1,2.5c-0.9,0.7-1.8,1.3-2.8,1.8c-1,0.5-2,0.9-3.1,1.1c-1.1,0.3-2.1,0.4-3.1,0.4 c-2.7,0-5.3-0.5-7.7-1.5c-2.4-1-4.5-2.5-6.3-4.5c-1.8-2-3.2-4.5-4.3-7.5c-1.1-3-1.6-6.5-1.6-10.5c0-3.9,0.5-7.4,1.6-10.5 c1.1-3.1,2.5-5.7,4.3-7.8c1.8-2.1,3.9-3.8,6.3-4.9c2.4-1.1,4.9-1.7,7.5-1.7c0.9,0,1.8,0.1,2.8,0.3c1,0.2,1.9,0.5,2.9,0.9 c1,0.4,1.9,0.9,2.7,1.5c0.9,0.6,1.6,1.2,2.3,1.9V38.2h12.5c0,0.4-0.1,0.6-0.2,0.9c-0.1,0.2-0.3,0.4-0.4,0.6 c-0.1,0.2-0.3,0.4-0.5,0.6c-0.2,0.2-0.3,0.5-0.3,0.8V99.6z M99.8,81.5c0-1.9-0.1-3.8-0.4-5.6c-0.3-1.8-0.7-3.3-1.4-4.7 c-0.6-1.4-1.5-2.4-2.7-3.2c-1.1-0.8-2.6-1.2-4.4-1.2c-1.6,0-2.8,0.3-3.9,0.8c-1,0.5-2,1.3-2.9,2.3c-2.5,2.6-3.7,6.9-3.7,12.7 c0,2.8,0.3,5.1,0.9,7c0.6,1.9,1.4,3.4,2.4,4.6c1,1.2,2.1,2,3.3,2.5c1.2,0.5,2.3,0.8,3.5,0.8c1.4,0,2.6-0.3,3.8-0.9 c1.1-0.6,2.1-1.5,2.9-2.8c0.8-1.3,1.4-2.9,1.8-4.9C99.6,86.8,99.8,84.4,99.8,81.5z"}]
            [:path {:class "st1", :d "M146.1,57.8c2.7,0,5.3,0.5,7.7,1.5c2.4,1,4.5,2.4,6.3,4.3c1.8,1.9,3.2,4.3,4.2,7.1c1,2.8,1.6,6.1,1.6,9.8 c0,0.8,0,1.6-0.1,2.4c-0.1,0.8-0.1,1.5-0.2,2.3h-30.4c0.3,2.2,0.8,4.1,1.6,5.7c0.8,1.6,1.7,2.9,2.8,4c1.1,1.1,2.3,1.8,3.7,2.3 c1.3,0.5,2.7,0.8,4.1,0.8c2.1,0,4.2-0.5,6.2-1.4c2-1,3.8-2.4,5.2-4.2l6.2,6.2c-4.7,5.6-10.5,8.3-17.6,8.3c-3.3,0-6.4-0.6-9.2-1.6 c-2.8-1.1-5.3-2.7-7.3-4.8c-2.1-2.1-3.7-4.6-4.8-7.6c-1.2-3-1.7-6.4-1.7-10.2c0-3.8,0.5-7.3,1.6-10.4c1.1-3.1,2.6-5.7,4.6-7.8 c2-2.1,4.3-3.8,6.9-4.9C140,58.4,142.9,57.8,146.1,57.8z M154.8,77c0-1.5-0.2-2.8-0.7-4.1s-1.1-2.5-1.8-3.4c-0.8-1-1.8-1.8-2.9-2.4 c-1.1-0.6-2.4-0.9-3.9-0.9c-1.4,0-2.6,0.3-3.8,0.8c-1.1,0.5-2.2,1.2-3,2.2c-0.9,0.9-1.6,2.1-2.1,3.4c-0.5,1.3-0.9,2.8-1.1,4.5H154.8 z"}]
            [:path {:class "st1", :d "M205.8,37.4c6.2,0,11.3,2.5,15.4,7.6l-4.6,8.9c-0.5,0-0.9-0.2-1-0.5c-0.2-0.3-0.4-0.9-0.6-1.6 c-0.4-0.6-0.9-1.2-1.5-1.8c-0.6-0.6-1.3-1.2-2.1-1.8c-0.8-0.5-1.7-1-2.7-1.4c-1-0.4-2-0.6-3.1-0.6c-2.8,0-4.8,1-6.2,3.1 c-1.4,2.1-2,5.3-2,9.6v2.4h14.2v8.8h-14.2v35.1h-10.8V70.2h-10v-8.8h10V58c0-3.4,0.5-6.4,1.5-9c1-2.6,2.4-4.7,4.1-6.5 c1.7-1.7,3.8-3,6.1-3.9C200.7,37.8,203.2,37.4,205.8,37.4z"}]
            [:path {:class "st1", :d "M254.9,73.9c-0.3,0-0.5-0.1-0.6-0.2c-0.1-0.1-0.3-0.3-0.3-0.5c-0.1-0.2-0.2-0.4-0.2-0.7 c-0.1-0.2-0.1-0.5-0.2-0.7c-0.6-0.7-1.3-1.4-2.2-2.1c-0.9-0.7-1.8-1.2-2.9-1.7c-1.1-0.5-2.2-0.9-3.3-1.2c-1.2-0.3-2.4-0.4-3.5-0.4 c-1,0-2,0.1-2.9,0.2c-0.9,0.1-1.7,0.4-2.4,0.7c-0.7,0.3-1.2,0.7-1.6,1.3c-0.4,0.5-0.6,1.1-0.6,1.9c0,0.8,0.2,1.5,0.7,2.1 c0.4,0.6,1.1,1.1,2,1.6c0.9,0.5,2,0.9,3.3,1.3c1.3,0.4,2.9,0.9,4.7,1.4c5.5,1.6,9.6,3.5,12.2,6s3.9,5.2,3.9,8.3c0,2.1-0.5,4.1-1.4,6 c-0.9,1.9-2.3,3.5-4,5c-1.7,1.4-3.8,2.6-6.2,3.4c-2.4,0.8-5.2,1.3-8.2,1.3c-3.9,0-7.5-0.6-10.8-1.9c-3.3-1.2-6.4-3.2-9.3-6l5.8-10.4 c0.4,0.1,0.7,0.3,0.9,0.7c0.2,0.4,0.4,1,0.7,1.7c0.5,0.8,1.3,1.7,2.2,2.4c1,0.8,2.1,1.5,3.2,2.2c1.2,0.6,2.4,1.2,3.8,1.6 s2.6,0.6,3.8,0.6c2.6,0,4.7-0.4,6.2-1.1c1.5-0.8,2.2-1.9,2.2-3.4c0-0.8-0.2-1.5-0.5-2c-0.3-0.6-0.8-1.1-1.5-1.6 c-0.7-0.5-1.6-1-2.7-1.4c-1.1-0.4-2.5-0.9-4.2-1.4c-0.9-0.3-1.8-0.5-2.7-0.8c-0.9-0.3-1.8-0.6-2.7-0.9c-1.8-0.7-3.4-1.4-4.8-2.3 c-1.4-0.9-2.6-1.8-3.6-2.9c-1-1.1-1.7-2.3-2.3-3.6c-0.5-1.3-0.8-2.8-0.8-4.5c0-1.6,0.5-3.3,1.4-4.9c0.9-1.6,2.2-3.1,3.8-4.4 c1.6-1.3,3.5-2.4,5.8-3.2c2.2-0.8,4.7-1.2,7.4-1.2c3.5,0,6.8,0.6,9.8,1.9c3,1.3,5.8,3.3,8.3,6.2L254.9,73.9z"}]
            [:path {:class "st1", :d "M294,57.8c0.9,0,1.9,0.1,3,0.3c1.1,0.2,2.1,0.5,3.1,1c1,0.4,1.9,0.9,2.8,1.5c0.9,0.6,1.6,1.2,2.2,1.9v-3.7h10.7 v69.7h-11.1V102c-0.6,0.7-1.4,1.4-2.3,2c-0.9,0.6-1.8,1.1-2.7,1.5c-1,0.4-1.9,0.7-2.9,0.9c-1,0.2-2,0.3-2.9,0.3 c-2.7,0-5.3-0.6-7.6-1.8c-2.4-1.2-4.4-2.9-6.2-5.1c-1.8-2.2-3.2-4.8-4.2-7.9c-1-3-1.6-6.4-1.6-10c0-3.7,0.5-7,1.5-9.9 c1-3,2.4-5.5,4.1-7.6c1.7-2.1,3.8-3.7,6.2-4.9C288.6,58.4,291.2,57.8,294,57.8z M295.1,66.9c-1.3,0-2.5,0.3-3.7,1 c-1.1,0.7-2.2,1.6-3,2.9c-0.9,1.3-1.6,2.8-2.1,4.6c-0.5,1.8-0.8,3.9-0.8,6.2c0,2.4,0.3,4.5,0.8,6.4c0.5,1.9,1.2,3.6,2.1,5 c0.9,1.4,2,2.4,3.2,3.2c1.2,0.8,2.5,1.1,3.9,1.1c2.8,0,5.1-1.3,6.6-3.8c1.6-2.6,2.4-6.4,2.4-11.6c0-2.7-0.2-4.9-0.7-6.8 c-0.4-1.9-1.1-3.5-1.9-4.7c-0.8-1.2-1.8-2.1-3-2.7C297.8,67.1,296.5,66.9,295.1,66.9z"}]
            [:path {:class "st1", :d "M371.7,58.9v40.8c0,0.8,0.1,1.7,0.3,2.7c0.2,0.9,0.4,1.9,0.7,2.9h-10.8c-0.4-0.3-0.6-0.7-0.8-1.3 c-0.1-0.6-0.2-1.1-0.2-1.6v-1.5c-0.7,1-1.5,1.9-2.5,2.7c-1,0.8-2,1.4-3.2,2c-1.1,0.5-2.3,1-3.5,1.3c-1.2,0.3-2.3,0.4-3.4,0.4 c-2.4,0-4.6-0.4-6.5-1.1c-1.9-0.7-3.5-1.8-4.8-3.4c-1.3-1.6-2.3-3.6-3-6c-0.7-2.5-1-5.4-1-8.9v-29h11v29c0,3.6,0.6,6.1,1.8,7.8 c1.2,1.6,2.8,2.4,4.8,2.4c1.4,0,2.7-0.3,3.9-0.8c1.2-0.6,2.3-1.3,3.2-2.4c0.9-1,1.6-2.2,2.1-3.6c0.5-1.4,0.8-2.9,0.8-4.6V58.9H371.7 z"}]
            [:path {:class "st1", :d "M389.5,65.4c4.8-5,10.6-7.6,17.6-7.6c2.9,0,5.6,0.4,7.9,1.1c2.3,0.7,4.3,1.9,5.9,3.5c1.6,1.6,2.9,3.6,3.7,6 c0.9,2.4,1.3,5.3,1.3,8.6v28.2h-10.6V101c-1.7,1.6-3.8,3-6.2,3.9c-2.4,1-4.8,1.5-7.2,1.5c-2.7,0-5.1-0.4-7.1-1.1 c-2-0.7-3.6-1.7-4.9-2.9c-1.3-1.2-2.3-2.7-2.9-4.3c-0.6-1.6-0.9-3.4-0.9-5.2c0-2.4,0.5-4.6,1.5-6.6c1-2,2.5-3.7,4.5-5.1 c2-1.4,4.4-2.6,7.3-3.4c2.9-0.8,6.2-1.2,10-1.2h5.9c-0.1-3.5-0.8-6.1-2.1-7.7c-1.4-1.6-3.5-2.4-6.4-2.4c-3.8,0-7.6,2.1-11.5,6.2 L389.5,65.4z M411.4,84.9c-2.9-0.1-5.3,0.1-7.2,0.5c-1.9,0.4-3.3,1-4.4,1.7c-1.1,0.7-1.8,1.5-2.2,2.4c-0.4,0.9-0.6,1.8-0.6,2.7 c0,0.8,0.2,1.5,0.5,2.2c0.4,0.7,0.8,1.3,1.5,1.8c0.6,0.5,1.3,0.9,2.2,1.2c0.8,0.3,1.7,0.5,2.6,0.5c1.5,0,2.9-0.3,4.1-0.8 c1.2-0.5,2.3-1.2,3.2-1.9c0.9-0.7,1.7-1.5,2.3-2.3c0.6-0.8,1.1-1.5,1.4-2.1c0.3-0.5,0.5-1.2,0.6-2.2c0.1-1,0.2-2.2,0.2-3.6 L411.4,84.9z"}]
            [:path {:class "st1", :d "M442.7,58.9h11.4v5c0.7-0.8,1.6-1.6,2.5-2.4c0.9-0.7,1.9-1.4,3-1.9c1.1-0.5,2.2-1,3.4-1.3 c1.2-0.3,2.4-0.5,3.6-0.5c2.6,0,5.1,0.4,7.5,1.2c2.4,0.8,4.5,2.1,6.3,3.9l-5.2,10.8c-1.6-2.2-3.2-3.9-4.7-4.9c-1.5-1-3-1.6-4.5-1.6 c-1.7,0-3.3,0.4-4.8,1.1c-1.5,0.7-2.8,1.7-3.9,3c-1.1,1.2-2,2.7-2.6,4.4c-0.6,1.7-0.9,3.4-0.9,5.3v24.2h-11.2V58.9z"}]
            [:path {:class "st1", :d "M509.1,57.8c2.7,0,5.3,0.5,7.7,1.5c2.4,1,4.5,2.4,6.3,4.3c1.8,1.9,3.2,4.3,4.2,7.1c1,2.8,1.6,6.1,1.6,9.8 c0,0.8,0,1.6-0.1,2.4c-0.1,0.8-0.1,1.5-0.2,2.3h-30.4c0.3,2.2,0.8,4.1,1.6,5.7c0.8,1.6,1.7,2.9,2.8,4c1.1,1.1,2.3,1.8,3.7,2.3 c1.3,0.5,2.7,0.8,4.1,0.8c2.1,0,4.2-0.5,6.2-1.4c2-1,3.8-2.4,5.2-4.2l6.2,6.2c-4.7,5.6-10.5,8.3-17.6,8.3c-3.3,0-6.4-0.6-9.2-1.6 c-2.8-1.1-5.3-2.7-7.3-4.8c-2.1-2.1-3.7-4.6-4.8-7.6c-1.2-3-1.7-6.4-1.7-10.2c0-3.8,0.5-7.3,1.6-10.4c1.1-3.1,2.6-5.7,4.6-7.8 c2-2.1,4.3-3.8,6.9-4.9C503,58.4,505.9,57.8,509.1,57.8z M517.7,77c0-1.5-0.2-2.8-0.7-4.1c-0.4-1.3-1.1-2.5-1.8-3.4 c-0.8-1-1.8-1.8-2.9-2.4c-1.1-0.6-2.4-0.9-3.9-0.9c-1.4,0-2.6,0.3-3.8,0.8c-1.1,0.5-2.2,1.2-3,2.2c-0.9,0.9-1.6,2.1-2.1,3.4 c-0.5,1.3-0.9,2.8-1.1,4.5H517.7z"}]
            [:path {:class "st0", :d "M579,29.3v85h-30v-8.7h19.8V38.3h-19.7v-9H579z"}]]]]]

        ;; cards
        [:div.flex.flex-col.w-0.flex-1.overflow-hidden
         (into
           [:div {:class "mt-8"}]
           (map (fn [[ns building-blocks]]
                  [:div {:class "max-w-8xl mx-auto px-4 sm:px-6 lg:px-8 mt-5"}
                   [:h2 {:class "text-lg leading-6 font-medium text-gray-900"} ns]
                   (into [:div {:class "mt-2 grid grid-cols-3 gap-4 sm:grid-cols-4 lg:grid-cols-5"}]
                         (map #(->card % {:on-click-fn (fn [] (swap! state assoc :selected-building-block-panel %))}) building-blocks))])
                (group-by :ns context)))

         ;; building block details panel
         (when-let [selected-building-block (:selected-building-block-panel @state)]
           (building-block-panel selected-building-block {:close-fn        (fn [] (swap! state assoc :selected-building-block-panel nil))
                                                          :on-click-ref-fn (fn [ref] (swap! state assoc :selected-building-block-panel (some (fn [{:keys [ns name] :as building-block}] (when (= (keyword ns name) ref) building-block)) context)))}))]]])))